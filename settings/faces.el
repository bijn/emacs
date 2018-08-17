;; Bijan Sondossi
;; faces.el
;; My custom faces.

(set-face-attribute 'default nil
                    :family "Courier New"
                    :height 90
                    :weight 'normal)

(cond ((eq system-type 'darwin)
       (set-face-attribute 'default nil :height 120)
       (let ((default-family "Andale Mono")
             (iosevka-font "Iosevka Term")
             (iosevka-weight 'light))
         (cond ((not (null (x-list-fonts iosevka-font)))
                (set-face-attribute 'default nil
                                    :font iosevka-font
                                    :weight iosevka-weight))
               (t (set-face-attribute 'default nil
                                      :family default-family)))))
      ((eq system-type 'gnu/linux)
       (let ((os-info "/etc/os-release")
             (arch-os "arch")
             (arch-family "Adobe Courier")
             (ubuntu-os "ubuntu")
             (ubuntu-family "Monospace")
             (ubuntu-height 100))
         (dolist (line (read-lines os-info))
           (cond ((string-match-p arch-os line)
                  (set-face-attribute 'default nil :family arch-family))
                 ((string-match-p ubuntu-os line)
                  (set-face-attribute 'default nil
                                      :family ubuntu-family
                                      :height ubuntu-height)))))))

;; end faces.el
