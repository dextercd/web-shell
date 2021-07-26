#include <utility>

#include <gd100/terminal.hpp>

[[clang::import_module("terminal")]]
[[clang::import_name("set_size")]]
void set_size(int width, int height);

[[clang::import_module("terminal")]]
[[clang::import_name("set_scroll_change")]]
void set_scroll_change(int scroll_change);

[[clang::import_module("terminal")]]
[[clang::import_name("set_glyph")]]
void set_glyph(
    int x, int y, std::uint32_t codepoint,
    int fg_r, int fg_g, int fg_b,
    int bg_r, int bg_g, int bg_b);

[[clang::import_module("terminal")]]
[[clang::import_name("set_cursor")]]
void set_cursor(int x, int y);

gd100::terminal terminal{{132, 32}};
gd100::terminal_instructee instructee{&terminal};
gd100::decoder decoder;

[[clang::export_name("alloc")]]
char* alloc(std::size_t count)
{
    return new char[count];
}

[[clang::export_name("dealloc")]]
void dealloc(char* ptr)
{
    delete ptr;
}

[[clang::export_name("do_decode")]]
void do_decode(char const* bytes, int count)
{
    decoder.decode(bytes, count, instructee);
}

[[clang::export_name("push_changes")]]
void push_changes()
{
    set_size(terminal.screen.size().width, terminal.screen.size().height);

    auto const scroll = terminal.screen.changed_scroll(); 
    if (scroll) {
        set_scroll_change(scroll);
    }

    auto const line_count = terminal.screen.size().height;
    auto const line_width = terminal.screen.size().width;
    for (int line = 0; line != line_count; ++line) {
        auto const& line_data = terminal.screen.lines[line];
        if (line_data.changed) {
            for (int x = 0; x != line_width; ++x) {
                auto const glyph = line_data.glyphs[x];
                auto style = glyph.style;

                if (style.mode.is_set(gd100::glyph_attr_bit::reversed)) {
                    std::swap(style.fg, style.bg);
                }

                set_glyph(x, line, glyph.code,
                    (unsigned)style.fg.r, (unsigned)style.fg.g, (unsigned)style.fg.b,
                    (unsigned)style.bg.r, (unsigned)style.bg.g, (unsigned)style.bg.b);
            }
        }
    }

    set_cursor(terminal.cursor.pos.x, terminal.cursor.pos.y);

    terminal.screen.clear_changes();
}

[[clang::export_name("bracketed_paste")]]
bool bracketed_paste()
{
    return terminal.mode.is_set(gd100::terminal_mode_bit::bracketed_paste);
}
