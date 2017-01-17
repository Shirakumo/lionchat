## About LionChat
This is a Qt GUI client for the [Lichat protocol](https://github.com/Shirakumo/lichat-protocol). It offers multiple connections, customisable looks, private conversations, user muting, autojoining, tray support, desktop notifications, highlighting, and so forth.

## How To
Load Lionchat and start it up:

```
(ql:quickload :lionchat)
(lionchat:start)
```

You can also build a standalone binary like so:

```
(asdf:operate :program-op :lionchat)
```

## Screenshots
Here's some screenshots to pique your interest:

### Basic Look
![basic](https://filebox.tymoon.eu/file/TVRJME5RPT0=)

### Style Options
![style](https://filebox.tymoon.eu/file/TVRJME5nPT0=)

### Connection Settings
![settings](https://filebox.tymoon.eu/file/TVRJME53PT0=)

### Customisable Interface, REPL
![uirepl](https://filebox.tymoon.eu/file/TVRJME9BPT0=)

### Private Conversations
![privmsg](https://filebox.tymoon.eu/file/TVRJME9RPT0=)

### Desktop Notifications
![notification](https://filebox.tymoon.eu/file/TVRJMU1BPT0=)
