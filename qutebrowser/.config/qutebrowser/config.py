import catpuccin

config.load_autoconfig(False)

catpuccin.setup(c, "macchiato", False)

c.keyhint.delay = 0

config.bind("<Ctrl-s>", "config-clear ;; config-source")

config.bind("<Ctrl-n>", "prompt-item-focus next", mode="prompt")
config.bind("<Ctrl-p>", "prompt-item-focus prev", mode="prompt")

config.bind("<Ctrl-n>", "completion-item-focus next", mode="command")
config.bind("<Ctrl-p>", "completion-item-focus prev", mode="command")

config.bind("<Alt-n>", "command-history-next", mode="command")
config.bind("<Alt-p>", "command-history-prev", mode="command")

config.bind("<Space>m", "spawn mpv {url}")
config.bind("<Space>M", "hint links spawn mpv {hint-url}")

c.auto_save.session = True
c.changelog_after_upgrade = "never"

c.confirm_quit = ["downloads"]
c.content.autoplay = False
c.content.notifications.presenter = "libnotify"

c.tabs.mousewheel_switching = False
c.tabs.title.format = "{audio}{perc_raw}% [{index}] {current_title}"

c.spellcheck.languages = ["en-US", "ru-RU"]
