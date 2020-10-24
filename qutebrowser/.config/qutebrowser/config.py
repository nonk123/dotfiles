c.auto_save.session = True # convenient

c.confirm_quit = ["downloads"]

c.content.autoplay = False # laggy af
c.content.headers.do_not_track = True
c.content.geolocation = "ask"
c.content.plugins = True

# Download without asking.
c.downloads.location.directory = "~/Downloads/"
c.downloads.location.prompt = False

# Wait for key combo continuation indefinitely.
c.input.partial_timeout = 0

c.scrolling.smooth = False # laggy
c.scrolling.bar = "never" # ugly

c.tabs.mousewheel_switching = False # annoying

c.url.start_pages = ["https://google.com/"]
c.url.searchengines["DEFAULT"] = "https://google.com/search?q={}"

c.tabs.title.format = "{index}. {current_title} ({perc_raw}%, {protocol})"

# Use Tab for Esc, like in `modal-mode`.
config.bind("<Tab>", "leave-mode", mode="insert")
config.bind("<Tab>", "leave-mode", mode="caret")

# Emacs's "cancel" key.
config.bind("<Ctrl-g>", "leave-mode", mode="command")

# `modal-mode` imitation.
config.bind("<Ctrl-n>", "scroll-page 0  0.5")
config.bind("<Ctrl-p>", "scroll-page 0 -0.5")

# Helm-like completions in command mode.
config.bind("<Ctrl-n>", "completion-item-focus next", mode="command")
config.bind("<Ctrl-p>", "completion-item-focus prev", mode="command")
config.bind("<Alt-n>", "command-history-next", mode="command")
config.bind("<Alt-p>", "command-history-prev", mode="command")

# Reload config.
config.bind("<space>c", "config-source")
# Download a YouTube video as a music track.
config.bind("<space>d", "spawn mpd-control download {url}")
# Look up on Wiktionary.
config.bind("<space>w", "set-cmd-text :open -t en.wiktionary.org/wiki/")

# One-keystroke bookmarks.
quickies = [("g", c.url.start_pages[0]), # first and only start page
            ("y", "youtube.com"),
            ("v", "vk.com"),
            ("x", "xhamster.desi")] # don't tell anybody

for key, url in quickies:
    config.bind(f"<space>{key}", f"open -t {url}")
