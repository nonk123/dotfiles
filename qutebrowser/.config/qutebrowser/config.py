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
