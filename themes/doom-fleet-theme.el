;;; doom-fleet-theme.el --- inspired by Atom One Dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 23, 2016 (28620647f838)
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Maintainer: Henrik Lissner <https://github.com/hlissner>
;; Source: https://github.com/atom/one-dark-ui
;;
;;; Commentary:
;;
;; This themepack's flagship theme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-fleet-theme nil
  "Options for the `doom-fleet' theme."
  :group 'doom-themes)

(defcustom doom-fleet-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-fleet-theme
  :type 'boolean)

(defcustom doom-fleet-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-fleet-theme
  :type 'boolean)

(defcustom doom-fleet-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-fleet-theme
  :type '(choice integer boolean))

(setq doom-fleet-padded-modeline t)
;;
;;; Theme definition

(def-doom-theme doom-fleet
  "Trying to hack together my own theme"

  ;; name        default   256           16
  ((bg         '("#141414" "black"       "black"  ))
   (fg         '("#d1d1d1" "#bfbfbf"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#111111" "black"       "black"        ))
   (fg-alt     '("#d1d1d1" "#2d2d2d"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#151515" "black"       "black"        ))
   (base1      '("#191a1c" "#1e1e1e"     "brightblack"  ))
   (base2      '("#202020" "#2e2e2e"     "brightblack"  ))
   (base3      '("#2a2a2a" "#262626"     "brightblack"  ))
   (base4      '("#454647" "#3f3f3f"     "brightblack"  ))
   (base5      '("#5e6163" "#525252"     "brightblack"  ))
   (base6      '("#747677" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9d9fa0" "#979797"     "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))

   (grey       base4)
   (red        '("#ff6c6b" "#ff6655" "red"          ))
   (orange     '("#ECA775" "#dd8844" "brightred"    ))
   (green      '("#AFCB85" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#E5C995" "#ECBE7B" "yellow"       ))
   (blue       '("#94C1FA" "#51afef" "brightblue"   ))
   (dark-blue  '("#163764" "#2257A0" "blue"         ))
   (magenta    '("#AC9CF9" "#c678dd" "brightmagenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
   (pink       '("#D898D8" "#a9a1e1" "magenta"      ))
   (cyan       '("#82CEBD" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   base3)
   (selection      dark-blue)
   (builtin        magenta)
   (constants      yellow)
   (doc-comments   (doom-lighten base5 0.25))
   (comments       base5)
   (functions      yellow)
   (keywords       cyan)
   (methods        cyan)
   (operators      blue)
   (type           blue)
   (strings        pink)
   (variables      magenta)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (doom-lighten bg 0.05))
   (modeline-bg-alt          `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg)))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-fleet-padded-modeline
      (if (integerp doom-fleet-padded-modeline) doom-fleet-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :slant 'italic)
   ((font-lock-function-name-face &override)
    :slant 'italic)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-fleet-brighter-modeline base8 highlight))
   (link :foreground yellow :underline t :weight 'bold)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-fleet-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground fg)
   ;;   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ((markdown-code-face &override) :background bg-alt)

   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

   ;;;;; highlight-indent-guides-mode
   ;; Tree sitter
   ;; Use tree-sitter-hl-face:CAPTURE-GROUP
   (tree-sitter-hl-face:type.builtin :foreground blue)
   )

  ;;;; Base theme variable overrides-
  ())


;;; doom-fleet-theme.el ends here
