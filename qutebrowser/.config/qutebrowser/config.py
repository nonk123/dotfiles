c.auto_save.session = True

c.confirm_quit = ["downloads"]

c.content.autoplay = False
c.content.headers.do_not_track = True
c.content.geolocation = "ask"
c.content.plugins = True

c.downloads.location.directory = "~/Downloads"
c.downloads.location.prompt = False

c.input.partial_timeout = 0

c.scrolling.smooth = False
c.scrolling.bar = "never"

c.search.ignore_case = "always"

c.tabs.mousewheel_switching = False

c.url.start_pages = ["https://google.com/"]
c.url.searchengines["DEFAULT"] = "https://google.com/search?q={}"

c.tabs.title.format = "{index}. {current_title} ({perc_raw}%, {protocol})"

config.bind("<Tab>", "leave-mode", mode="insert")
config.bind("<Tab>", "leave-mode", mode="caret")

config.bind("<Ctrl-g>", "leave-mode", mode="command")

config.bind("<Ctrl-n>", "scroll-page 0  0.5")
config.bind("<Ctrl-p>", "scroll-page 0 -0.5")

config.bind("<Ctrl-n>", "completion-item-focus next", mode="command")
config.bind("<Ctrl-p>", "completion-item-focus prev", mode="command")
config.bind("<Alt-n>", "command-history-next", mode="command")
config.bind("<Alt-p>", "command-history-prev", mode="command")

config.bind("<space>c", "config-source")
config.bind("<space>d", "spawn mpd-control download {url}")
config.bind("<space>g", "open -t google.com")
config.bind("<space>y", "open -t youtube.com")
config.bind("<space>w", "set-cmd-text :open -t en.wiktionary.org/wiki/")
