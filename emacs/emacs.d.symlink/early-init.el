(unless (and (display-graphic-p) sys/macp)
  (push '(menu-bar-lines . 0) default-frame-alist))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
