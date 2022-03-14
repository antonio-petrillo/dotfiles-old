---------------------------
-- Default awesome theme --
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
--local themes_path = gfs.get_themes_dir()
local themes_path = string.format("%s/.config/awesome/themes/catppuccin/", os.getenv("HOME"))


local theme = {}



theme.font_only     = "JetBrains Mono Nerd Font"
theme.font          = "JetBrains Mono Nerd Font 9"

theme.flamingo      = "#f2cdcd"
theme.mauve         = "#dd86f2"
theme.pink          = "#f5c2e7"
theme.maroon        = "#e8a2af"
theme.red           = "#f28fad"
theme.peach         = "#f8bd96"
theme.yellow        = "#fae3b0"
theme.green         = "#abe9b3"
theme.teal          = "#b5e8e0"
theme.blue          = "#96cdfb"
theme.sky           = "#89dceb"

theme.black_0       = "#161320"
theme.black_1       = "#1a1826"
theme.black_2       = "#1e1e2e"
theme.black_3       = "#302d41"
theme.black_4       = "#575268"

theme.gray_0        = "#6e6c7e"
theme.gray_1        = "#988ba2"
theme.gray_2        = "#c3bac6"

theme.white         = "#d9e0ee"
theme.lavender      = "#c9cbff"
theme.rosewater     = "#f5e0dc"

theme.bg_normal     = theme.black_0
theme.bg_focus      = theme.black_1
theme.bg_minimize   = theme.black_2
theme.bg_urgent     = theme.grey_1
theme.bg_systray    = theme.black_1

theme.fg_normal     = theme.pink
theme.fg_focus      = theme.lavender
theme.fg_urgent     = theme.red
theme.fg_minimize   = theme.peach

theme.useless_gap   = dpi(5)
theme.border_width  = dpi(3)
theme.border_normal = theme.black_0
theme.border_focus  = theme.lavender
theme.border_marked = theme.green


-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"


-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path.."titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."titlebar/maximized_focus_active.png"

--theme.wallpaper = themes_path.."default/background.png"
theme.wallpaper = themes_path.."vaporWave.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."layouts/fairhw.png"
theme.layout_fairv = themes_path.."layouts/fairvw.png"
theme.layout_floating  = themes_path.."layouts/floatingw.png"
theme.layout_magnifier = themes_path.."layouts/magnifierw.png"
theme.layout_max = themes_path.."layouts/maxw.png"
theme.layout_fullscreen = themes_path.."layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."layouts/tileleftw.png"
theme.layout_tile = themes_path.."layouts/tilew.png"
theme.layout_tiletop = themes_path.."layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."layouts/spiralw.png"
theme.layout_dwindle = themes_path.."layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."layouts/cornernww.png"
theme.layout_cornerne = themes_path.."layouts/cornernew.png"
theme.layout_cornersw = themes_path.."layouts/cornersww.png"
theme.layout_cornerse = themes_path.."layouts/cornersew.png"
theme.layout_cascade = themes_path.."layouts/cascade.png"
theme.layout_cascadetile = themes_path.."layouts/cascadetile.png"
theme.layout_cascadetilew = themes_path.."layouts/cascadetilew.png"
theme.layout_cascadew = themes_path.."layouts/cascadew.png"
theme.layout_centerwork = themes_path.."layouts/centerwork.png"
theme.layout_centerfair = themes_path.."layouts/centerfair.png"
theme.layout_centerfairw = themes_path.."layouts/centerfairw.png"
theme.layout_centerwork = themes_path.."layouts/centerwork.png"
theme.layout_centerworkh = themes_path.."layouts/centerworkh.png"
theme.layout_centerworkhw = themes_path.."layouts/centerworkhw.png"
theme.layout_centerworkw = themes_path.."layouts/centerworkw.png"
theme.layout_termfair = themes_path.."layouts/termfair.png"
theme.layout_termfairw = themes_path.."layouts/termfairw.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "Papirus-Dark"

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80