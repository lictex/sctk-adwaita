use std::error::Error;
use std::num::NonZeroU32;
use std::sync::Arc;

use tiny_skia::{
    ClipMask, Color, FillRule, IntRect, Path, PathBuilder, Pixmap, PixmapMut, PixmapPaint, Point,
    Rect, Stroke, Transform,
};

use smithay_client_toolkit::reexports::client::protocol::wl_shm;
use smithay_client_toolkit::reexports::client::protocol::wl_subsurface::WlSubsurface;
use smithay_client_toolkit::reexports::client::protocol::wl_surface::WlSurface;
use smithay_client_toolkit::reexports::client::{Dispatch, Proxy, QueueHandle};

use smithay_client_toolkit::compositor::SurfaceData;
use smithay_client_toolkit::shell::xdg::frame::{DecorationsFrame, FrameAction, FrameClick};
use smithay_client_toolkit::shell::xdg::window::{WindowManagerCapabilities, WindowState};
use smithay_client_toolkit::shell::WaylandSurface;
use smithay_client_toolkit::shm::{slot::SlotPool, Shm};
use smithay_client_toolkit::subcompositor::SubcompositorState;
use smithay_client_toolkit::subcompositor::SubsurfaceData;

mod buttons;
mod config;
mod parts;
mod pointer;
pub mod theme;
mod title;

use crate::theme::{
    ColorMap, ColorTheme, BORDER_SIZE, CORNER_RADIUS, HEADER_SIZE, VISIBLE_BORDER_SIZE,
};

use buttons::Buttons;
use parts::DecorationParts;
use pointer::{Location, MouseState};
use title::TitleText;

/// XXX this is not result, so `must_use` when needed.
type SkiaResult = Option<()>;

const SHADOW_SRC: &[u8] = include_bytes!("frame_shadow.png");
const SHADOW_SIZE: i32 = 64;
const SHADOW_BOTTOM_MARGIN: i32 = 29;
const SHADOW_TOP_MARGIN: i32 = 23;
const SHADOW_SIDE_MARGIN: i32 = 26;

/// A simple set of decorations
#[derive(Debug)]
pub struct AdwaitaFrame<State> {
    /// The base surface used to create the window.
    base_surface: WlSurface,

    /// Subcompositor to create/drop subsurfaces ondemand.
    subcompositor: Arc<SubcompositorState>,

    /// Queue handle to perform object creation.
    queue_handle: QueueHandle<State>,

    /// The drawable decorations, `None` when hidden.
    decorations: Option<DecorationParts>,

    /// Memory pool to allocate the buffers for the decorations.
    pool: SlotPool,

    /// Whether the frame should be redrawn.
    dirty: bool,

    /// Wether the frame is resizable.
    resizable: bool,

    buttons: Buttons,
    state: WindowState,
    wm_capabilities: WindowManagerCapabilities,
    mouse: MouseState,
    theme: ColorTheme,
    title: Option<String>,
    title_text: Option<TitleText>,

    top_shadow: Pixmap,
    bottom_shadow: Pixmap,
    side_shadow: Pixmap,
    top_corner_shadow: Pixmap,
    bottom_corner_shadow: Pixmap,
}

impl<State> AdwaitaFrame<State>
where
    State: Dispatch<WlSurface, SurfaceData> + Dispatch<WlSubsurface, SubsurfaceData> + 'static,
{
    pub fn new(
        base_surface: &impl WaylandSurface,
        shm: &Shm,
        subcompositor: Arc<SubcompositorState>,
        queue_handle: QueueHandle<State>,
        frame_config: FrameConfig,
    ) -> Result<Self, Box<dyn Error>> {
        let base_surface = base_surface.wl_surface().clone();
        let pool = SlotPool::new(1, shm)?;

        let decorations = Some(DecorationParts::new(
            &base_surface,
            &subcompositor,
            &queue_handle,
        ));

        let theme = frame_config.theme;

        let shadow_src = tiny_skia::Pixmap::decode_png(SHADOW_SRC).unwrap();
        assert_eq!((shadow_src.width(), shadow_src.height()), (64, 128));

        let pixels = shadow_src.data().chunks_exact(4).collect::<Vec<_>>();
        let top_pixels = &pixels[..(SHADOW_SIZE * SHADOW_SIZE) as _];
        let bottom_pixels = &pixels[(SHADOW_SIZE * SHADOW_SIZE) as _..];

        let top_corner_shadow = Pixmap::from_vec(
            top_pixels
                .iter()
                .copied()
                .flatten()
                .copied()
                .collect::<Vec<_>>(),
            IntRect::from_xywh(0, 0, SHADOW_SIZE as _, SHADOW_SIZE as _)
                .unwrap()
                .size(),
        )
        .unwrap();
        let bottom_corner_shadow = Pixmap::from_vec(
            bottom_pixels
                .iter()
                .copied()
                .flatten()
                .copied()
                .collect::<Vec<_>>(),
            IntRect::from_xywh(0, 0, SHADOW_SIZE as _, SHADOW_SIZE as _)
                .unwrap()
                .size(),
        )
        .unwrap();
        let side_shadow = Pixmap::from_vec(
            (&top_pixels[top_pixels.len() - SHADOW_SIZE as usize..])
                .iter()
                .copied()
                .flatten()
                .copied()
                .collect::<Vec<_>>(),
            IntRect::from_xywh(0, 0, SHADOW_SIZE as _, 1)
                .unwrap()
                .size(),
        )
        .unwrap();
        let bottom_shadow = Pixmap::from_vec(
            bottom_pixels
                .chunks_exact(SHADOW_SIZE as _)
                .map(|f| *f.last().unwrap())
                .flatten()
                .copied()
                .collect::<Vec<_>>(),
            IntRect::from_xywh(0, 0, 1, SHADOW_SIZE as _)
                .unwrap()
                .size(),
        )
        .unwrap();
        let top_shadow = Pixmap::from_vec(
            top_pixels
                .chunks_exact(SHADOW_SIZE as _)
                .map(|f| *f.last().unwrap())
                .flatten()
                .copied()
                .collect::<Vec<_>>(),
            IntRect::from_xywh(0, 0, 1, SHADOW_SIZE as _)
                .unwrap()
                .size(),
        )
        .unwrap();

        Ok(AdwaitaFrame {
            base_surface,
            decorations,
            pool,
            subcompositor,
            queue_handle,
            dirty: true,
            title: None,
            title_text: TitleText::new(theme.active.font_color),
            theme,
            buttons: Default::default(),
            mouse: Default::default(),
            state: WindowState::empty(),
            wm_capabilities: WindowManagerCapabilities::all(),
            resizable: true,
            top_shadow,
            bottom_shadow,
            side_shadow,
            top_corner_shadow,
            bottom_corner_shadow,
        })
    }

    /// Update the current frame config.
    pub fn set_config(&mut self, config: FrameConfig) {
        self.theme = config.theme;
        self.dirty = true;
    }

    fn precise_location(&self, location: Location, height: u32, x: f64, y: f64) -> Location {
        match location {
            Location::Head | Location::Button(_) => self.buttons.find_button(x, y),
            Location::Left | Location::TopLeft | Location::BottomLeft => {
                if y <= f64::from(BORDER_SIZE) {
                    Location::TopLeft
                } else if y >= f64::from(height - BORDER_SIZE) {
                    Location::BottomLeft
                } else {
                    Location::Left
                }
            }
            Location::Right | Location::TopRight | Location::BottomRight => {
                if y <= f64::from(BORDER_SIZE) {
                    Location::TopRight
                } else if y >= f64::from(height - BORDER_SIZE) {
                    Location::BottomRight
                } else {
                    Location::Right
                }
            }
            other => other,
        }
    }

    fn redraw_inner(&mut self) -> SkiaResult {
        let decorations = self.decorations.as_mut()?;

        // Reset the dirty bit.
        self.dirty = false;

        // Don't draw borders if the frame explicitly hidden or fullscreened.
        if self.state.contains(WindowState::FULLSCREEN) {
            decorations.hide();
            return Some(());
        }

        let colors = if self.state.contains(WindowState::ACTIVATED) {
            &self.theme.active
        } else {
            &self.theme.inactive
        };

        let draw_borders = if self.state.contains(WindowState::MAXIMIZED) {
            // Don't draw the borders.
            decorations.hide_borders();
            false
        } else {
            true
        };
        let border_paint = colors.border_paint();

        // Draw the borders.
        for (idx, part) in decorations
            .parts()
            .filter(|(idx, _)| *idx == DecorationParts::HEADER || draw_borders)
        {
            let scale = part.scale();

            // XXX to perfectly align the visible borders we draw them with
            // the header, otherwise rounded corners won't look 'smooth' at the
            // start. To achieve that, we enlargen the width of the header by
            // 2 * `VISIBLE_BORDER_SIZE`, and move `x` by `VISIBLE_BORDER_SIZE`
            // to the left.
            let (width, height, pos) = if idx == DecorationParts::HEADER && draw_borders {
                (
                    (part.width + 2 * VISIBLE_BORDER_SIZE) * scale,
                    part.height * scale,
                    (part.pos.0 - VISIBLE_BORDER_SIZE as i32, part.pos.1),
                )
            } else {
                (part.width * scale, part.height * scale, part.pos)
            };

            let (buffer, canvas) = match self.pool.create_buffer(
                width as i32,
                height as i32,
                width as i32 * 4,
                wl_shm::Format::Argb8888,
            ) {
                Ok((buffer, canvas)) => (buffer, canvas),
                Err(_) => continue,
            };

            // Create the pixmap and fill with transparent color.
            let pixmap = &mut PixmapMut::from_bytes(canvas, width, height)?;

            // Fill everything with transparent background, since we draw rounded corners and
            // do invisible borders to enlarge the input zone.
            pixmap.fill(Color::TRANSPARENT);

            // The visible border is one pt.
            let visible_border_size = VISIBLE_BORDER_SIZE * scale;

            let draw_shadow_part = |dst: &mut PixmapMut,
                                    src: &Pixmap,
                                    scale_x: i32,
                                    scale_y: i32,
                                    translate_x: i32,
                                    translate_y: i32| {
                dst.draw_pixmap(
                    0, // this seems to have rounding issues
                    0, // so use transforms instead
                    src.as_ref(),
                    &PixmapPaint {
                        opacity: colors.shadow_opacity,
                        ..Default::default()
                    },
                    Transform::default()
                        .post_scale(scale as _, scale as _)
                        .post_scale(scale_x as _, scale_y as _)
                        .post_translate(
                            (translate_x * scale as i32) as f32,
                            (translate_y * scale as i32) as f32,
                        ),
                    None,
                );
            };

            match idx {
                DecorationParts::HEADER => {
                    if let Some(title_text) = self.title_text.as_mut() {
                        title_text.update_scale(scale);
                        title_text.update_color(colors.font_color);
                    }

                    draw_headerbar(
                        pixmap,
                        self.title_text.as_ref().map(|t| t.pixmap()).unwrap_or(None),
                        scale as f32,
                        self.resizable,
                        &self.state,
                        &self.theme,
                        &self.buttons,
                        self.mouse.location,
                    );
                }
                DecorationParts::LEFT => {
                    draw_shadow_part(
                        pixmap,
                        &self.side_shadow,
                        1,
                        part.height as i32
                            - BORDER_SIZE as i32 * 2
                            - (SHADOW_SIZE - SHADOW_TOP_MARGIN)
                            - (SHADOW_SIZE - SHADOW_BOTTOM_MARGIN),
                        BORDER_SIZE as i32 - SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 + (SHADOW_SIZE - SHADOW_TOP_MARGIN),
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.top_corner_shadow,
                        1,
                        1,
                        BORDER_SIZE as i32 - SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 - SHADOW_TOP_MARGIN,
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.bottom_corner_shadow,
                        1,
                        1,
                        BORDER_SIZE as i32 - SHADOW_SIDE_MARGIN,
                        part.height as i32
                            - BORDER_SIZE as i32
                            - (SHADOW_SIZE - SHADOW_BOTTOM_MARGIN),
                    );
                    pixmap.fill_rect(
                        {
                            let x = (pos.0.unsigned_abs() * scale) - visible_border_size;
                            let y = pos.1.unsigned_abs() * scale;
                            Rect::from_xywh(
                                x as f32,
                                y as f32,
                                visible_border_size as f32,
                                (height - y - BORDER_SIZE * scale + visible_border_size) as f32,
                            )
                            .unwrap()
                        },
                        &border_paint,
                        Transform::identity(),
                        None,
                    );
                }
                DecorationParts::RIGHT => {
                    draw_shadow_part(
                        pixmap,
                        &self.side_shadow,
                        -1,
                        part.height as i32
                            - BORDER_SIZE as i32 * 2
                            - (SHADOW_SIZE - SHADOW_TOP_MARGIN)
                            - (SHADOW_SIZE - SHADOW_BOTTOM_MARGIN),
                        SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 + (SHADOW_SIZE - SHADOW_TOP_MARGIN),
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.top_corner_shadow,
                        -1,
                        1,
                        SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 - SHADOW_TOP_MARGIN,
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.bottom_corner_shadow,
                        -1,
                        1,
                        SHADOW_SIDE_MARGIN,
                        part.height as i32
                            - BORDER_SIZE as i32
                            - (SHADOW_SIZE - SHADOW_BOTTOM_MARGIN),
                    );
                    pixmap.fill_rect(
                        {
                            let y = pos.1.unsigned_abs() * scale;
                            Rect::from_xywh(
                                0.,
                                y as f32,
                                visible_border_size as f32,
                                (height - y - BORDER_SIZE * scale + visible_border_size) as f32,
                            )
                            .unwrap()
                        },
                        &border_paint,
                        Transform::identity(),
                        None,
                    );
                }
                DecorationParts::BOTTOM => {
                    draw_shadow_part(
                        pixmap,
                        &self.bottom_shadow,
                        part.width as i32 - (SHADOW_SIZE - SHADOW_SIDE_MARGIN) * 2,
                        1,
                        SHADOW_SIZE - SHADOW_SIDE_MARGIN,
                        -SHADOW_SIZE + SHADOW_BOTTOM_MARGIN,
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.bottom_corner_shadow,
                        1,
                        1,
                        -SHADOW_SIDE_MARGIN,
                        -SHADOW_SIZE + SHADOW_BOTTOM_MARGIN,
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.bottom_corner_shadow,
                        -1,
                        1,
                        part.width as i32 + SHADOW_SIDE_MARGIN,
                        -SHADOW_SIZE + SHADOW_BOTTOM_MARGIN,
                    );
                    pixmap.fill_rect(
                        Rect::from_xywh(0., 0., width as f32, visible_border_size as f32).unwrap(),
                        &border_paint,
                        Transform::identity(),
                        None,
                    );
                }
                DecorationParts::TOP => {
                    draw_shadow_part(
                        pixmap,
                        &self.top_shadow,
                        part.width as i32 - (SHADOW_SIZE - SHADOW_SIDE_MARGIN) * 2,
                        1,
                        SHADOW_SIZE - SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 - SHADOW_TOP_MARGIN,
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.top_corner_shadow,
                        1,
                        1,
                        -SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 - SHADOW_TOP_MARGIN,
                    );
                    draw_shadow_part(
                        pixmap,
                        &self.top_corner_shadow,
                        -1,
                        1,
                        part.width as i32 + SHADOW_SIDE_MARGIN,
                        BORDER_SIZE as i32 - SHADOW_TOP_MARGIN,
                    );
                }
                _ => {}
            };

            part.surface.set_buffer_scale(scale as i32);

            part.subsurface.set_position(pos.0, pos.1);
            buffer.attach_to(&part.surface).ok()?;

            if part.surface.version() >= 4 {
                part.surface.damage_buffer(0, 0, i32::MAX, i32::MAX);
            } else {
                part.surface.damage(0, 0, i32::MAX, i32::MAX);
            }

            part.surface.commit();
        }

        Some(())
    }
}

impl<State> DecorationsFrame for AdwaitaFrame<State>
where
    State: Dispatch<WlSurface, SurfaceData> + Dispatch<WlSubsurface, SubsurfaceData> + 'static,
{
    fn update_state(&mut self, state: WindowState) {
        let difference = self.state.symmetric_difference(state);
        self.state = state;
        self.dirty |= difference.intersects(
            WindowState::ACTIVATED
                | WindowState::FULLSCREEN
                | WindowState::MAXIMIZED
                | WindowState::TILED,
        );
    }

    fn update_wm_capabilities(&mut self, wm_capabilities: WindowManagerCapabilities) {
        self.dirty |= self.wm_capabilities != wm_capabilities;
        self.wm_capabilities = wm_capabilities;
        self.buttons.update(wm_capabilities);
    }

    fn set_hidden(&mut self, hidden: bool) {
        if hidden {
            self.dirty = false;
            let _ = self.pool.resize(1);
            self.decorations = None;
        } else if self.decorations.is_none() {
            self.decorations = Some(DecorationParts::new(
                &self.base_surface,
                &self.subcompositor,
                &self.queue_handle,
            ));
            self.dirty = true;
        }
    }

    fn set_resizable(&mut self, resizable: bool) {
        self.dirty |= self.resizable != resizable;
        self.resizable = resizable;
    }

    fn resize(&mut self, width: NonZeroU32, height: NonZeroU32) {
        let decorations = self
            .decorations
            .as_mut()
            .expect("trying to resize the hidden frame.");

        decorations.resize(width.get(), height.get());
        self.buttons.arrange(width.get());
        self.dirty = true;
    }

    fn draw(&mut self) {
        self.redraw_inner();
    }

    fn subtract_borders(
        &self,
        width: NonZeroU32,
        height: NonZeroU32,
    ) -> (Option<NonZeroU32>, Option<NonZeroU32>) {
        if self.decorations.is_none() || self.state.contains(WindowState::FULLSCREEN) {
            (Some(width), Some(height))
        } else {
            (
                Some(width),
                NonZeroU32::new(height.get().saturating_sub(HEADER_SIZE)),
            )
        }
    }

    fn add_borders(&self, width: u32, height: u32) -> (u32, u32) {
        if self.decorations.is_none() || self.state.contains(WindowState::FULLSCREEN) {
            (width, height)
        } else {
            (width, height + HEADER_SIZE)
        }
    }

    fn location(&self) -> (i32, i32) {
        if self.decorations.is_none() || self.state.contains(WindowState::FULLSCREEN) {
            (0, 0)
        } else {
            (0, -(HEADER_SIZE as i32))
        }
    }

    fn set_title(&mut self, title: impl Into<String>) {
        let new_title = title.into();
        if let Some(title_text) = self.title_text.as_mut() {
            title_text.update_title(new_title.clone());
        }

        self.title = Some(new_title);
        self.dirty = true;
    }

    fn on_click(&mut self, click: FrameClick, pressed: bool) -> Option<FrameAction> {
        match click {
            FrameClick::Normal => {
                self.mouse
                    .click(pressed, self.resizable, &self.state, &self.wm_capabilities)
            }
            FrameClick::Alternate => self.mouse.alternate_click(pressed, &self.wm_capabilities),
        }
    }

    fn click_point_moved(&mut self, surface: &WlSurface, x: f64, y: f64) -> Option<&str> {
        let decorations = self.decorations.as_ref()?;
        let Some((location, part)) = decorations.find_surface(surface) else { return None };

        let old_location = self.mouse.location;

        let location = self.precise_location(location, part.height, x, y);
        let new_cursor = self.mouse.moved(location, x, y, self.resizable);

        // Set dirty if we moved the cursor between the buttons.
        self.dirty |= (matches!(old_location, Location::Button(_))
            || matches!(self.mouse.location, Location::Button(_)))
            && old_location != self.mouse.location;

        Some(new_cursor)
    }

    fn click_point_left(&mut self) {
        self.mouse.left()
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn is_hidden(&self) -> bool {
        self.decorations.is_none()
    }
}

/// The configuration for the [`AdwaitaFrame`] frame.
#[derive(Debug, Clone)]
pub struct FrameConfig {
    pub theme: ColorTheme,
}

impl FrameConfig {
    /// Create the new configuration with the given `theme`.
    pub fn new(theme: ColorTheme) -> Self {
        Self { theme }
    }

    /// This is equivalent of calling `FrameConfig::new(ColorTheme::auto())`.
    ///
    /// For details see [`ColorTheme::auto`].
    pub fn auto() -> Self {
        Self {
            theme: ColorTheme::auto(),
        }
    }

    /// This is equivalent of calling `FrameConfig::new(ColorTheme::light())`.
    ///
    /// For details see [`ColorTheme::light`].
    pub fn light() -> Self {
        Self {
            theme: ColorTheme::light(),
        }
    }

    /// This is equivalent of calling `FrameConfig::new(ColorTheme::dark())`.
    ///
    /// For details see [`ColorTheme::dark`].
    pub fn dark() -> Self {
        Self {
            theme: ColorTheme::dark(),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn draw_headerbar(
    pixmap: &mut PixmapMut,
    text_pixmap: Option<&Pixmap>,
    scale: f32,
    resizable: bool,
    state: &WindowState,
    theme: &ColorTheme,
    buttons: &Buttons,
    mouse: Location,
) {
    let colors = theme.for_state(state.contains(WindowState::ACTIVATED));

    let _ = draw_headerbar_bg(pixmap, scale, colors, state);

    // Horizontal margin.
    let margin_h = if state.intersects(WindowState::MAXIMIZED | WindowState::TILED) {
        0.
    } else {
        VISIBLE_BORDER_SIZE as f32 * scale
    };

    if let Some(text_pixmap) = text_pixmap {
        const TEXT_OFFSET: f32 = 10.;

        let closest_button_x = buttons.left_most().x() * scale;
        let offset_x = TEXT_OFFSET * scale;

        let canvas_w = pixmap.width() as f32;
        let canvas_h = pixmap.height() as f32;

        let header_w = canvas_w - margin_h * 2.0;
        let header_h = canvas_h;

        let text_w = text_pixmap.width() as f32;
        let text_h = text_pixmap.height() as f32;

        let x = margin_h + header_w / 2. - text_w / 2.;
        let y = header_h / 2. - text_h / 2.;

        let (x, y) = if x + text_w < closest_button_x - offset_x {
            (x, y)
        } else {
            let x = closest_button_x - text_w - offset_x;
            let y = header_h / 2. - text_h / 2.;
            (x, y)
        };

        // Ensure that text start within the bounds.
        let x = x.max(margin_h + offset_x);

        if let Some(clip) = Rect::from_xywh(0., 0., closest_button_x - offset_x, canvas_h) {
            let mut mask = ClipMask::new();
            mask.set_path(
                canvas_w as u32,
                canvas_h as u32,
                &PathBuilder::from_rect(clip),
                FillRule::Winding,
                false,
            );
            pixmap.draw_pixmap(
                x as i32,
                y as i32,
                text_pixmap.as_ref(),
                &PixmapPaint::default(),
                Transform::identity(),
                Some(&mask),
            );
        }
    }

    // Draw the buttons.
    for button in buttons.iter().flatten() {
        // Ensure that it'll be visible.
        if button.x() > margin_h {
            button.draw(scale, colors, mouse, pixmap, resizable, state);
        }
    }
}

#[must_use]
fn draw_headerbar_bg(
    pixmap: &mut PixmapMut,
    scale: f32,
    colors: &ColorMap,
    state: &WindowState,
) -> SkiaResult {
    let w = pixmap.width() as f32;
    let h = pixmap.height() as f32;

    let radius = if state.intersects(WindowState::MAXIMIZED | WindowState::TILED) {
        0.
    } else {
        CORNER_RADIUS as f32 * scale
    };

    pixmap.fill_path(
        &rounded_headerbar_shape(0., 0., w, h, radius)?,
        &colors.headerbar_paint(),
        FillRule::Winding,
        Transform::identity(),
        None,
    );

    pixmap.stroke_path(
        &rounded_headerbar_shape(
            scale / 2.,
            scale / 2.,
            w - scale,
            h - scale,
            radius - scale / 2.,
        )?,
        &colors.border_paint(),
        &Stroke {
            width: VISIBLE_BORDER_SIZE as f32 * scale,
            ..Default::default()
        },
        Transform::identity(),
        None,
    );

    pixmap.fill_rect(
        Rect::from_xywh(0., h - 1., w, h)?,
        &colors.border_paint(),
        Transform::identity(),
        None,
    );

    Some(())
}

fn rounded_headerbar_shape(x: f32, y: f32, width: f32, height: f32, radius: f32) -> Option<Path> {
    use std::f32::consts::FRAC_1_SQRT_2;

    let mut pb = PathBuilder::new();
    let mut cursor = Point::from_xy(x, y);

    // !!!
    // This code is heavily "inspired" by https://gitlab.com/snakedye/snui/
    // So technically it should be licensed under MPL-2.0, sorry about that 🥺 👉👈
    // !!!

    // Positioning the cursor
    cursor.y += radius;
    pb.move_to(cursor.x, cursor.y);

    // Drawing the outline
    pb.cubic_to(
        cursor.x,
        cursor.y,
        cursor.x,
        cursor.y - FRAC_1_SQRT_2 * radius,
        {
            cursor.x += radius;
            cursor.x
        },
        {
            cursor.y -= radius;
            cursor.y
        },
    );
    pb.line_to(
        {
            cursor.x = x + width - radius;
            cursor.x
        },
        cursor.y,
    );
    pb.cubic_to(
        cursor.x,
        cursor.y,
        cursor.x + FRAC_1_SQRT_2 * radius,
        cursor.y,
        {
            cursor.x += radius;
            cursor.x
        },
        {
            cursor.y += radius;
            cursor.y
        },
    );
    pb.line_to(cursor.x, {
        cursor.y = y + height;
        cursor.y
    });
    pb.line_to(
        {
            cursor.x = x;
            cursor.x
        },
        cursor.y,
    );

    pb.close();

    pb.finish()
}
