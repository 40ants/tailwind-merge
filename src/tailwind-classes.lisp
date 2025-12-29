(uiop:define-package #:tailwind-merge/tailwind-classes
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of
                #:->
                #:dict)
  (:import-from #:tailwind-merge/validators
                #:colorp
                #:integer-value-p
                #:number-value-p
                #:fraction-value-p
                #:tshirt-size-p
                #:percent-value-p
                #:empty-or-number-p
                #:make-validators-from-rule)
  (:import-from #:tailwind-merge/arbitrary
                #:arbitrary-value-p
                #:arbitrary-variable-p
                #:arbitrary-number-p
                #:arbitrary-length-p))
(in-package #:tailwind-merge/tailwind-classes)


;;; ==================
;;; Class Groups Definition
;;; ==================
;;; Based on tailwind-merge default-config.ts
;;;
;;; Structure:
;;; - (:class-group-id pattern1 pattern2 ...)
;;; - Patterns can be:
;;;   - Keywords for exact class name matches (e.g., :block, :flex)
;;;   - Lists with prefix and validators: (:prefix validator1 validator2)
;;;     - null validator means exact prefix match with no suffix
;;;     - function validators check the suffix value

(defparameter *classes*
  '(
    ;; ==============
    ;; --- Layout ---
    ;; ==============

    ;; Aspect Ratio
    (:aspect (:aspect :auto :square :video)
     (:aspect fraction-value-p))

    ;; Container (deprecated in v4)
    (:container :container)

    ;; Columns
    (:columns (:columns number-value-p)
     (:columns tshirt-size-p))

    ;; Break After
    (:break-after (:break-after :auto :avoid :all :avoid-page :page :left :right :column))

    ;; Break Before
    (:break-before (:break-before :auto :avoid :all :avoid-page :page :left :right :column))

    ;; Break Inside
    (:break-inside (:break-inside :auto :avoid :avoid-page :avoid-column))

    ;; Box Decoration Break
    (:box-decoration (:box-decoration :slice :clone))

    ;; Box Sizing
    (:box (:box :border :content))

    ;; Display
    (:display (:block :inline-block :inline :flex :inline-flex
                      :table :inline-table :table-caption :table-cell
                      :table-column :table-column-group :table-footer-group
                      :table-header-group :table-row-group :table-row
                      :flow-root :grid :inline-grid :contents :list-item :hidden))

    ;; Screen Reader
    (:sr :sr-only :not-sr-only)

    ;; Float
    (:float (:float :right :left :none :start :end))

    ;; Clear
    (:clear (:clear :left :right :both :none :start :end))

    ;; Isolation
    (:isolation :isolate :isolation-auto)

    ;; Object Fit
    (:object-fit (:object :contain :cover :fill :none :scale-down))

    ;; Object Position
    (:object-position (:object :center :top :bottom :left :right
                       :top-left :left-top :top-right :right-top
                       :bottom-right :right-bottom :bottom-left :left-bottom))

    ;; Overflow
    (:overflow (:overflow :auto :hidden :clip :visible :scroll))
    (:overflow-x (:overflow-x :auto :hidden :clip :visible :scroll))
    (:overflow-y (:overflow-y :auto :hidden :clip :visible :scroll))

    ;; Overscroll
    (:overscroll (:overscroll :auto :contain :none))
    (:overscroll-x (:overscroll-x :auto :contain :none))
    (:overscroll-y (:overscroll-y :auto :contain :none))

    ;; Position
    (:position :static :fixed :absolute :relative :sticky)

    ;; Top/Right/Bottom/Left (Inset)
    (:inset (:inset :auto :full)
     (:inset number-value-p)
     (:inset fraction-value-p))
    (:inset-x (:inset-x :auto :full)
     (:inset-x number-value-p)
     (:inset-x fraction-value-p))
    (:inset-y (:inset-y :auto :full)
     (:inset-y number-value-p)
     (:inset-y fraction-value-p))
    (:start (:start :auto :full)
     (:start number-value-p)
     (:start fraction-value-p))
    (:end (:end :auto :full)
     (:end number-value-p)
     (:end fraction-value-p))
    (:top (:top :auto :full)
     (:top number-value-p)
     (:top fraction-value-p))
    (:right (:right :auto :full)
     (:right number-value-p)
     (:right fraction-value-p))
    (:bottom (:bottom :auto :full)
     (:bottom number-value-p)
     (:bottom fraction-value-p))
    (:left (:left :auto :full)
     (:left number-value-p)
     (:left fraction-value-p))

    ;; Visibility
    (:visibility :visible :invisible :collapse)

    ;; Z-Index
    (:z (:z :auto)
     (:z integer-value-p))

    ;; ========================
    ;; --- Flexbox and Grid ---
    ;; ========================

    ;; Flex Basis
    (:basis (:basis :auto :full)
     (:basis number-value-p)
     (:basis fraction-value-p)
     (:basis tshirt-size-p))

    ;; Flex Direction
    (:flex-direction (:flex :row :row-reverse :col :col-reverse))

    ;; Flex Wrap
    (:flex-wrap (:flex :nowrap :wrap :wrap-reverse))

    ;; Flex
    (:flex (:flex :auto :initial :none)
     (:flex number-value-p)
     (:flex fraction-value-p))

    ;; Flex Grow
    (:grow (:grow null)
     (:grow number-value-p)
     (:grow arbitrary-value-p)
     (:grow arbitrary-variable-p))

    ;; Flex Shrink
    (:shrink (:shrink null)
     (:shrink number-value-p))

    ;; Order
    (:order (:order :first :last :none)
     (:order integer-value-p))

    ;; Grid Template Columns
    (:grid-cols (:grid-cols :none :subgrid)
     (:grid-cols integer-value-p))

    ;; Grid Column Start/End
    (:col-start-end (:col :auto :full)
     (:col integer-value-p)
     (:col-span :full)
     (:col-span integer-value-p))
    (:col-start (:col-start :auto)
     (:col-start integer-value-p))
    (:col-end (:col-end :auto)
     (:col-end integer-value-p))

    ;; Grid Template Rows
    (:grid-rows (:grid-rows :none :subgrid)
     (:grid-rows integer-value-p))

    ;; Grid Row Start/End
    (:row-start-end (:row :auto :full)
     (:row integer-value-p)
     (:row-span :full)
     (:row-span integer-value-p))
    (:row-start (:row-start :auto)
     (:row-start integer-value-p))
    (:row-end (:row-end :auto)
     (:row-end integer-value-p))

    ;; Grid Auto Flow
    (:grid-flow (:grid-flow :row :col :dense :row-dense :col-dense))

    ;; Grid Auto Columns/Rows
    (:auto-cols (:auto-cols :auto :min :max :fr))
    (:auto-rows (:auto-rows :auto :min :max :fr))

    ;; Gap
    (:gap (:gap number-value-p))
    (:gap-x (:gap-x number-value-p))
    (:gap-y (:gap-y number-value-p))

    ;; Justify Content
    (:justify-content (:justify :start :end :center :between :around :evenly :stretch :baseline :normal
                       :center-safe :end-safe))

    ;; Justify Items
    (:justify-items (:justify-items :start :end :center :stretch :normal :center-safe :end-safe))

    ;; Justify Self
    (:justify-self (:justify-self :auto :start :end :center :stretch :center-safe :end-safe))

    ;; Align Content
    (:align-content (:content :normal :start :end :center :between :around :evenly :stretch :baseline
                     :center-safe :end-safe))

    ;; Align Items
    (:align-items (:items :start :end :center :stretch :baseline :center-safe :end-safe)
     (:items-baseline null)
     (:items-baseline :last))

    ;; Align Self
    (:align-self (:self :auto :start :end :center :stretch :baseline :center-safe :end-safe)
     (:self-baseline null)
     (:self-baseline :last))

    ;; Place Content
    (:place-content (:place-content :start :end :center :between :around :evenly :stretch :baseline
                                    :center-safe :end-safe))

    ;; Place Items
    (:place-items (:place-items :start :end :center :stretch :baseline :center-safe :end-safe))

    ;; Place Self
    (:place-self (:place-self :auto :start :end :center :stretch :center-safe :end-safe))

    ;; ===============
    ;; --- Spacing ---
    ;; ===============

    ;; Padding
    (:p (:p number-value-p))
    (:px (:px number-value-p))
    (:py (:py number-value-p))
    (:ps (:ps number-value-p))
    (:pe (:pe number-value-p))
    (:pt (:pt number-value-p))
    (:pr (:pr number-value-p))
    (:pb (:pb number-value-p))
    (:pl (:pl number-value-p))

    ;; Margin
    (:m (:m :auto)
     (:m number-value-p))
    (:mx (:mx :auto)
      (:mx number-value-p))
    (:my (:my :auto)
     (:my number-value-p))
    (:ms (:ms :auto)
     (:ms number-value-p))
    (:me (:me :auto)
     (:me number-value-p))
    (:mt (:mt :auto)
     (:mt number-value-p))
    (:mr (:mr :auto)
     (:mr number-value-p))
    (:mb (:mb :auto)
     (:mb number-value-p))
    (:ml (:ml :auto)
     (:ml number-value-p))

    ;; Space Between
    (:space-x (:space-x number-value-p))
    (:space-x-reverse :space-x-reverse)
    (:space-y (:space-y number-value-p))
    (:space-y-reverse :space-y-reverse)

    ;; ==============
    ;; --- Sizing ---
    ;; ==============

    ;; Size (width & height)
    (:size (:size :auto :full :min :max :fit :dvw :dvh :lvw :lvh :svw :svh)
     (:size number-value-p)
     (:size fraction-value-p))

    ;; Width
    (:w (:w :auto :full :screen :min :max :fit :dvw :dvh :lvw :lvh :svw :svh)
     (:w number-value-p)
     (:w fraction-value-p)
     (:w tshirt-size-p))

    ;; Min-Width
    (:min-w (:min-w :auto :full :screen :none :min :max :fit)
     (:min-w number-value-p)
     (:min-w fraction-value-p)
     (:min-w tshirt-size-p))

    ;; Max-Width
    (:max-w (:max-w :auto :full :screen :none :prose :min :max :fit)
     (:max-w number-value-p)
     (:max-w fraction-value-p)
     (:max-w tshirt-size-p))

    ;; Height
    (:h (:h :auto :full :screen :lh :min :max :fit :dvh :dvw :lvh :lvw :svh :svw)
     (:h number-value-p)
     (:h fraction-value-p))

    ;; Min-Height
    (:min-h (:min-h :auto :full :screen :lh :none :min :max :fit)
     (:min-h number-value-p)
     (:min-h fraction-value-p))

    ;; Max-Height
    (:max-h (:max-h :auto :full :screen :lh :min :max :fit)
     (:max-h number-value-p)
     (:max-h fraction-value-p))

    ;; ==================
    ;; --- Typography ---
    ;; ==================

    ;; Font Size
    (:font-size (:text :base)
     (:text tshirt-size-p))

    ;; Font Smoothing
    (:font-smoothing :antialiased :subpixel-antialiased)

    ;; Font Style
    (:font-style :italic :not-italic)

    ;; Font Weight
    (:font-weight (:font :thin :extralight :light :normal :medium :semibold :bold :extrabold :black)
     (:font number-value-p))

    ;; Font Stretch
    (:font-stretch (:font-stretch :ultra-condensed :extra-condensed :condensed :semi-condensed
                                  :normal :semi-expanded :expanded :extra-expanded :ultra-expanded)
     (:font-stretch percent-value-p))

    ;; Font Family
    (:font-family (:font :sans :serif :mono))

    ;; Font Variant Numeric
    (:fvn-normal :normal-nums)
    (:fvn-ordinal :ordinal)
    (:fvn-slashed-zero :slashed-zero)
    (:fvn-figure :lining-nums :oldstyle-nums)
    (:fvn-spacing :proportional-nums :tabular-nums)
    (:fvn-fraction :diagonal-fractions :stacked-fractions)

    ;; Letter Spacing (Tracking)
    (:tracking (:tracking :tighter :tight :normal :wide :wider :widest))

    ;; Line Clamp
    (:line-clamp (:line-clamp :none)
     (:line-clamp number-value-p))

    ;; Line Height (Leading)
    (:leading (:leading :none :tight :snug :normal :relaxed :loose)
     (:leading number-value-p))

    ;; List Style Image
    (:list-image (:list-image :none))

    ;; List Style Position
    (:list-style-position (:list :inside :outside))

    ;; List Style Type
    (:list-style-type (:list :disc :decimal :none))

    ;; Text Alignment
    (:text-alignment (:text :left :center :right :justify :start :end))

    ;; Placeholder Color (deprecated in v3)
    (:placeholder-color (:placeholder)) ;; Would need color validator

    ;; Text Color
    ;; TODO: support arbitrary color values here
    (:text-color (:text colorp))

    ;; Text Decoration
    (:text-decoration :underline :overline :line-through :no-underline)

    ;; Text Decoration Style
    (:text-decoration-style (:decoration :solid :dashed :dotted :double :wavy))

    ;; Text Decoration Thickness
    (:text-decoration-thickness (:decoration :from-font :auto)
     (:decoration number-value-p))

    ;; Text Decoration Color
    (:text-decoration-color) ;; Would need color validator

    ;; Text Underline Offset
    (:underline-offset (:underline-offset :auto)
     (:underline-offset number-value-p))

    ;; Text Transform
    (:text-transform :uppercase :lowercase :capitalize :normal-case)

    ;; Text Overflow
    (:text-overflow :truncate :text-ellipsis :text-clip)

    ;; Text Wrap
    (:text-wrap (:text :wrap :nowrap :balance :pretty))

    ;; Text Indent
    (:indent (:indent number-value-p))

    ;; Vertical Align
    (:vertical-align (:align :baseline :top :middle :bottom :text-top :text-bottom :sub :super))

    ;; Whitespace
    (:whitespace (:whitespace :normal :nowrap :pre :pre-line :pre-wrap :break-spaces))

    ;; Word Break
    (:break (:break :normal :words :all :keep))

    ;; Overflow Wrap
    (:wrap (:wrap :break-word :anywhere :normal))

    ;; Hyphens
    (:hyphens (:hyphens :none :manual :auto))

    ;; Content
    (:content (:content :none))

    ;; ===================
    ;; --- Backgrounds ---
    ;; ===================

    ;; TODO: support arbitrary color values here
    (:bg-color (:bg colorp))
    
    ;; Background Attachment
    (:bg-attachment (:bg :fixed :local :scroll))

    ;; Background Clip
    (:bg-clip (:bg-clip :border :padding :content :text))

    ;; Background Origin
    (:bg-origin (:bg-origin :border :padding :content))

    ;; Background Position
    (:bg-position (:bg :center :top :bottom :left :right
                   :top-left :left-top :top-right :right-top
                   :bottom-right :right-bottom :bottom-left :left-bottom))

    ;; Background Repeat
    (:bg-repeat (:bg :no-repeat)
     (:bg-repeat null)
     (:bg-repeat :x :y :space :round))

    ;; Background Size
    (:bg-size (:bg :auto :cover :contain))

    ;; Background Image
    (:bg-image (:bg :none)
     (:bg-linear-to :t :tr :r :br :b :bl :l :tl)
     (:bg-radial null)
     (:bg-conic))

    ;; Background Color
    (:bg-color) ;; Would need color validator

    ;; Gradient Color Stops
    (:gradient-from-pos (:from percent-value-p))
    (:gradient-via-pos (:via percent-value-p))
    (:gradient-to-pos (:to percent-value-p))
    (:gradient-from) ;; Would need color validator
    (:gradient-via)  ;; Would need color validator
    (:gradient-to)   ;; Would need color validator

    ;; ===============
    ;; --- Borders ---
    ;; ===============

    ;; TODO: support arbitrary color values here
    (:border-color (:border colorp))
    
    ;; Border Radius
    (:rounded (:rounded null)
     (:rounded :none :full)
     (:rounded tshirt-size-p))
    (:rounded-s (:rounded-s null)
     (:rounded-s :none :full)
     (:rounded-s tshirt-size-p))
    (:rounded-e (:rounded-e null)
     (:rounded-e :none :full)
     (:rounded-e tshirt-size-p))
    (:rounded-t (:rounded-t null)
     (:rounded-t :none :full)
     (:rounded-t tshirt-size-p))
    (:rounded-r (:rounded-r null)
     (:rounded-r :none :full)
     (:rounded-r tshirt-size-p))
    (:rounded-b (:rounded-b null)
     (:rounded-b :none :full)
     (:rounded-b tshirt-size-p))
    (:rounded-l (:rounded-l null)
     (:rounded-l :none :full)
     (:rounded-l tshirt-size-p))
    (:rounded-ss (:rounded-ss null)
     (:rounded-ss :none :full)
     (:rounded-ss tshirt-size-p))
    (:rounded-se (:rounded-se null)
     (:rounded-se :none :full)
     (:rounded-se tshirt-size-p))
    (:rounded-ee (:rounded-ee null)
     (:rounded-ee :none :full)
     (:rounded-ee tshirt-size-p))
    (:rounded-es (:rounded-es null)
     (:rounded-es :none :full)
     (:rounded-es tshirt-size-p))
    (:rounded-tl (:rounded-tl null)
     (:rounded-tl :none :full)
     (:rounded-tl tshirt-size-p))
    (:rounded-tr (:rounded-tr null)
     (:rounded-tr :none :full)
     (:rounded-tr tshirt-size-p))
    (:rounded-br (:rounded-br null)
     (:rounded-br :none :full)
     (:rounded-br tshirt-size-p))
    (:rounded-bl (:rounded-bl null)
     (:rounded-bl :none :full)
     (:rounded-bl tshirt-size-p))

    ;; Border Width
    (:border-w (:border null)
     (:border number-value-p))
    (:border-w-x (:border-x null)
     (:border-x number-value-p))
    (:border-w-y (:border-y null)
     (:border-y number-value-p))
    (:border-w-s (:border-s null)
     (:border-s number-value-p))
    (:border-w-e (:border-e null)
     (:border-e number-value-p))
    (:border-w-t (:border-t null)
     (:border-t number-value-p))
    (:border-w-r (:border-r null)
     (:border-r number-value-p))
    (:border-w-b (:border-b null)
     (:border-b number-value-p))
    (:border-w-l (:border-l null)
     (:border-l number-value-p))

    ;; Divide Width
    (:divide-x (:divide-x null)
     (:divide-x number-value-p))
    (:divide-x-reverse :divide-x-reverse)
    (:divide-y (:divide-y null)
     (:divide-y number-value-p))
    (:divide-y-reverse :divide-y-reverse)

    ;; Border Style
    (:border-style (:border :solid :dashed :dotted :double :hidden :none))

    ;; Divide Style
    (:divide-style (:divide :solid :dashed :dotted :double :hidden :none))

    ;; Border Color
    (:border-color)   ;; Would need color validator
    (:border-color-x) ;; Would need color validator
    (:border-color-y) ;; Would need color validator
    (:border-color-s) ;; Would need color validator
    (:border-color-e) ;; Would need color validator
    (:border-color-t) ;; Would need color validator
    (:border-color-r) ;; Would need color validator
    (:border-color-b) ;; Would need color validator
    (:border-color-l) ;; Would need color validator

    ;; Divide Color
    (:divide-color) ;; Would need color validator

    ;; Outline Style
    (:outline-style (:outline :solid :dashed :dotted :double :none :hidden))

    ;; Outline Offset
    (:outline-offset (:outline-offset number-value-p))

    ;; Outline Width
    (:outline-w (:outline null)
     (:outline number-value-p))

    ;; Outline Color
    (:outline-color) ;; Would need color validator

    ;; ===============
    ;; --- Effects ---
    ;; ===============

    ;; Box Shadow
    (:shadow (:shadow null)
     (:shadow :none)
     (:shadow tshirt-size-p))

    ;; Box Shadow Color
    (:shadow-color) ;; Would need color validator

    ;; Inset Box Shadow
    (:inset-shadow (:inset-shadow :none)
     (:inset-shadow tshirt-size-p))

    ;; Inset Box Shadow Color
    (:inset-shadow-color) ;; Would need color validator

    ;; Ring Width
    (:ring-w (:ring null)
     (:ring number-value-p))
    (:ring-w-inset :ring-inset)

    ;; Ring Color
    (:ring-color) ;; Would need color validator

    ;; Ring Offset Width (deprecated in v4)
    (:ring-offset-w (:ring-offset number-value-p))

    ;; Ring Offset Color (deprecated in v4)
    (:ring-offset-color) ;; Would need color validator

    ;; Inset Ring Width
    (:inset-ring-w (:inset-ring null)
     (:inset-ring number-value-p))

    ;; Inset Ring Color
    (:inset-ring-color) ;; Would need color validator

    ;; Text Shadow
    (:text-shadow (:text-shadow :none)
     (:text-shadow tshirt-size-p))

    ;; Text Shadow Color
    (:text-shadow-color) ;; Would need color validator

    ;; Opacity
    (:opacity (:opacity number-value-p))

    ;; Mix Blend Mode
    (:mix-blend (:mix-blend :normal :multiply :screen :overlay :darken :lighten
                            :color-dodge :color-burn :hard-light :soft-light
                            :difference :exclusion :hue :saturation :color :luminosity
                            :plus-darker :plus-lighter))

    ;; Background Blend Mode
    (:bg-blend (:bg-blend :normal :multiply :screen :overlay :darken :lighten
                          :color-dodge :color-burn :hard-light :soft-light
                          :difference :exclusion :hue :saturation :color :luminosity))

    ;; ===============
    ;; --- Filters ---
    ;; ===============

    ;; Filter
    (:filter (:filter null)
      (:filter :none))

    ;; Blur
    (:blur (:blur null)
     (:blur :none)
     (:blur tshirt-size-p))

    ;; Brightness
    (:brightness (:brightness number-value-p))

    ;; Contrast
    (:contrast (:contrast number-value-p))

    ;; Drop Shadow
    (:drop-shadow (:drop-shadow null)
     (:drop-shadow :none)
     (:drop-shadow tshirt-size-p))

    ;; Drop Shadow Color
    (:drop-shadow-color) ;; Would need color validator

    ;; Grayscale
    (:grayscale (:grayscale null)
     (:grayscale number-value-p)
     (:grayscale arbitrary-value-p)
     (:grayscale arbitrary-variable-p))

    ;; Hue Rotate
    (:hue-rotate (:hue-rotate number-value-p))

    ;; Invert
    (:invert (:invert null)
     (:invert number-value-p))

    ;; Saturate
    (:saturate (:saturate number-value-p))

    ;; Sepia
    (:sepia (:sepia null)
     (:sepia number-value-p))

    ;; Backdrop Filter
    (:backdrop-filter (:backdrop-filter null)
     (:backdrop-filter :none))

    ;; Backdrop Blur
    (:backdrop-blur (:backdrop-blur null)
     (:backdrop-blur :none)
     (:backdrop-blur tshirt-size-p))

    ;; Backdrop Brightness
    (:backdrop-brightness (:backdrop-brightness number-value-p))

    ;; Backdrop Contrast
    (:backdrop-contrast (:backdrop-contrast number-value-p))

    ;; Backdrop Grayscale
    (:backdrop-grayscale (:backdrop-grayscale null)
     (:backdrop-grayscale number-value-p))

    ;; Backdrop Hue Rotate
    (:backdrop-hue-rotate (:backdrop-hue-rotate number-value-p))

    ;; Backdrop Invert
    (:backdrop-invert (:backdrop-invert null)
     (:backdrop-invert number-value-p))

    ;; Backdrop Opacity
    (:backdrop-opacity (:backdrop-opacity number-value-p))

    ;; Backdrop Saturate
    (:backdrop-saturate (:backdrop-saturate number-value-p))

    ;; Backdrop Sepia
    (:backdrop-sepia (:backdrop-sepia null)
     (:backdrop-sepia number-value-p))

    ;; ==============
    ;; --- Tables ---
    ;; ==============

    ;; Border Collapse
    (:border-collapse (:border :collapse :separate))

    ;; Border Spacing
    (:border-spacing (:border-spacing number-value-p))
    (:border-spacing-x (:border-spacing-x number-value-p))
    (:border-spacing-y (:border-spacing-y number-value-p))

    ;; Table Layout
    (:table-layout (:table :auto :fixed))

    ;; Caption Side
    (:caption (:caption :top :bottom))

    ;; =================================
    ;; --- Transitions and Animation ---
    ;; =================================

    ;; Transition Property
    (:transition (:transition null)
     (:transition :all :colors :opacity :shadow :transform :none))

    ;; Transition Behavior
    (:transition-behavior (:transition :normal :discrete))

    ;; Transition Duration
    (:duration (:duration :initial)
     (:duration number-value-p))

    ;; Transition Timing Function
    (:ease (:ease :linear :initial :in :out :in-out))

    ;; Transition Delay
    (:delay (:delay number-value-p))

    ;; Animation
    (:animate (:animate :none :spin :ping :pulse :bounce))

    ;; ==================
    ;; --- Transforms ---
    ;; ==================

    ;; Backface Visibility
    (:backface (:backface :hidden :visible))

    ;; Perspective
    (:perspective (:perspective :dramatic :near :normal :midrange :distant :none))

    ;; Perspective Origin
    (:perspective-origin (:perspective-origin :center :top :bottom :left :right
                          :top-left :left-top :top-right :right-top
                          :bottom-right :right-bottom :bottom-left :left-bottom))

    ;; Rotate
    (:rotate (:rotate :none)
     (:rotate number-value-p))
    (:rotate-x (:rotate-x :none)
     (:rotate-x number-value-p))
    (:rotate-y (:rotate-y :none)
     (:rotate-y number-value-p))
    (:rotate-z (:rotate-z :none)
     (:rotate-z number-value-p))

    ;; Scale
    (:scale (:scale :none)
     (:scale number-value-p))
    (:scale-x (:scale-x :none)
     (:scale-x number-value-p))
    (:scale-y (:scale-y :none)
     (:scale-y number-value-p))
    (:scale-z (:scale-z :none)
     (:scale-z number-value-p))
    (:scale-3d :scale-3d)

    ;; Skew
    (:skew (:skew number-value-p))
    (:skew-x (:skew-x number-value-p))
    (:skew-y (:skew-y number-value-p))

    ;; Transform
    (:transform (:transform null)
     (:transform :none :gpu :cpu))

    ;; Transform Origin
    (:transform-origin (:origin :center :top :bottom :left :right
                        :top-left :left-top :top-right :right-top
                        :bottom-right :right-bottom :bottom-left :left-bottom))

    ;; Transform Style
    (:transform-style (:transform :3d :flat))

    ;; Translate
    (:translate (:translate :full)
     (:translate number-value-p)
     (:translate fraction-value-p))
    (:translate-x (:translate-x :full)
     (:translate-x number-value-p)
     (:translate-x fraction-value-p))
    (:translate-y (:translate-y :full)
     (:translate-y number-value-p)
     (:translate-y fraction-value-p))
    (:translate-z (:translate-z :full)
     (:translate-z number-value-p)
     (:translate-z fraction-value-p))
    (:translate-none :translate-none)

    ;; =====================
    ;; --- Interactivity ---
    ;; =====================

    ;; Accent Color
    (:accent) ;; Would need color validator

    ;; Appearance
    (:appearance (:appearance :none :auto))

    ;; Caret Color
    (:caret-color) ;; Would need color validator

    ;; Color Scheme
    (:color-scheme (:scheme :normal :dark :light :light-dark :only-dark :only-light))

    ;; Cursor
    (:cursor (:cursor :auto :default :pointer :wait :text :move :help :not-allowed :none
              :context-menu :progress :cell :crosshair :vertical-text :alias :copy
                      :no-drop :grab :grabbing :all-scroll :col-resize :row-resize
                      :n-resize :e-resize :s-resize :w-resize :ne-resize :nw-resize
                      :se-resize :sw-resize :ew-resize :ns-resize :nesw-resize :nwse-resize
                      :zoom-in :zoom-out))

    ;; Field Sizing
    (:field-sizing (:field-sizing :fixed :content))

    ;; Pointer Events
    (:pointer-events (:pointer-events :auto :none))

    ;; Resize
    (:resize (:resize :none :y :x)
     (:resize null))

    ;; Scroll Behavior
    (:scroll-behavior (:scroll :auto :smooth))

    ;; Scroll Margin
    (:scroll-m (:scroll-m number-value-p))
    (:scroll-mx (:scroll-mx number-value-p))
    (:scroll-my (:scroll-my number-value-p))
    (:scroll-ms (:scroll-ms number-value-p))
    (:scroll-me (:scroll-me number-value-p))
    (:scroll-mt (:scroll-mt number-value-p))
    (:scroll-mr (:scroll-mr number-value-p))
    (:scroll-mb (:scroll-mb number-value-p))
    (:scroll-ml (:scroll-ml number-value-p))

    ;; Scroll Padding
    (:scroll-p (:scroll-p number-value-p))
    (:scroll-px (:scroll-px number-value-p))
    (:scroll-py (:scroll-py number-value-p))
    (:scroll-ps (:scroll-ps number-value-p))
    (:scroll-pe (:scroll-pe number-value-p))
    (:scroll-pt (:scroll-pt number-value-p))
    (:scroll-pr (:scroll-pr number-value-p))
    (:scroll-pb (:scroll-pb number-value-p))
    (:scroll-pl (:scroll-pl number-value-p))

    ;; Scroll Snap Align
    (:snap-align (:snap :start :end :center :align-none))

    ;; Scroll Snap Stop
    (:snap-stop (:snap :normal :always))

    ;; Scroll Snap Type
    (:snap-type (:snap :none :x :y :both))

    ;; Scroll Snap Strictness
    (:snap-strictness (:snap :mandatory :proximity))

    ;; Touch Action
    (:touch (:touch :auto :none :manipulation))
    (:touch-x (:touch-pan :x :left :right))
    (:touch-y (:touch-pan :y :up :down))
    (:touch-pz :touch-pinch-zoom)

    ;; User Select
    (:select (:select :none :text :all :auto))

    ;; Will Change
    (:will-change (:will-change :auto :scroll :contents :transform))

    ;; ===========
    ;; --- SVG ---
    ;; ===========

    ;; Fill
    (:fill (:fill :none)) ;; Would need color validator for other values

    ;; Stroke Width
    (:stroke-w (:stroke number-value-p)
     (:stroke arbitrary-number-p)
     (:stroke arbitrary-length-p)
     (:stroke arbitrary-variable-p))

    ;; Stroke
    (:stroke (:stroke :none)) ;; Would need color validator for other values

    ;; =====================
    ;; --- Accessibility ---
    ;; =====================

    ;; Forced Color Adjust
    (:forced-color-adjust (:forced-color-adjust :auto :none))))


;;; ============================
;;; Conflicting Class Groups
;;; ============================
;;; When a class from a group is applied, it should also override
;;; classes from all conflicting groups listed here.
;;;
;;; Format: (group-id conflicting-group-1 conflicting-group-2 ...)
;;;
;;; Example: When p-4 is applied (group :p), it should override
;;; classes from :px, :py, :ps, :pe, :pt, :pr, :pb, :pl groups.

(defparameter *conflicting-class-groups*
  '(;; Overflow
    (:overflow :overflow-x :overflow-y)
    (:overscroll :overscroll-x :overscroll-y)

    ;; Inset (position)
    (:inset :inset-x :inset-y :start :end :top :right :bottom :left)
    (:inset-x :right :left)
    (:inset-y :top :bottom)

    ;; Flex
    (:flex :basis :grow :shrink)

    ;; Gap
    (:gap :gap-x :gap-y)

    ;; Padding
    (:p :px :py :ps :pe :pt :pr :pb :pl)
    (:px :pr :pl)
    (:py :pt :pb)

    ;; Margin
    (:m :mx :my :ms :me :mt :mr :mb :ml)
    (:mx :mr :ml)
    (:my :mt :mb)

    ;; Size
    (:size :w :h)

    ;; Font size affects leading
    (:font-size :leading)

    ;; Font variant numeric
    (:fvn-normal :fvn-ordinal :fvn-slashed-zero :fvn-figure :fvn-spacing :fvn-fraction)
    (:fvn-ordinal :fvn-normal)
    (:fvn-slashed-zero :fvn-normal)
    (:fvn-figure :fvn-normal)
    (:fvn-spacing :fvn-normal)
    (:fvn-fraction :fvn-normal)

    ;; Line clamp affects display and overflow
    (:line-clamp :display :overflow)

    ;; Border radius
    (:rounded :rounded-s :rounded-e :rounded-t :rounded-r :rounded-b :rounded-l
              :rounded-ss :rounded-se :rounded-ee :rounded-es
              :rounded-tl :rounded-tr :rounded-br :rounded-bl)
    (:rounded-s :rounded-ss :rounded-es)
    (:rounded-e :rounded-se :rounded-ee)
    (:rounded-t :rounded-tl :rounded-tr)
    (:rounded-r :rounded-tr :rounded-br)
    (:rounded-b :rounded-br :rounded-bl)
    (:rounded-l :rounded-tl :rounded-bl)

    ;; Border spacing
    (:border-spacing :border-spacing-x :border-spacing-y)

    ;; Border width
    (:border-w :border-w-x :border-w-y :border-w-s :border-w-e
               :border-w-t :border-w-r :border-w-b :border-w-l)
    (:border-w-x :border-w-r :border-w-l)
    (:border-w-y :border-w-t :border-w-b)

    ;; Border color
    (:border-color :border-color-x :border-color-y :border-color-s :border-color-e
                   :border-color-t :border-color-r :border-color-b :border-color-l)
    (:border-color-x :border-color-r :border-color-l)
    (:border-color-y :border-color-t :border-color-b)

    ;; Translate
    (:translate :translate-x :translate-y :translate-none)
    (:translate-none :translate :translate-x :translate-y :translate-z)

    ;; Scroll margin
    (:scroll-m :scroll-mx :scroll-my :scroll-ms :scroll-me
               :scroll-mt :scroll-mr :scroll-mb :scroll-ml)
    (:scroll-mx :scroll-mr :scroll-ml)
    (:scroll-my :scroll-mt :scroll-mb)

    ;; Scroll padding
    (:scroll-p :scroll-px :scroll-py :scroll-ps :scroll-pe
               :scroll-pt :scroll-pr :scroll-pb :scroll-pl)
    (:scroll-px :scroll-pr :scroll-pl)
    (:scroll-py :scroll-pt :scroll-pb)

    ;; Touch
    (:touch :touch-x :touch-y :touch-pz)
    (:touch-x :touch)
    (:touch-y :touch)
    (:touch-pz :touch)))



(defun build-classes-map ()
  (loop with result = (serapeum:dict)
        for (class-name . rest) in *classes*
        for rules = (if rest
                        rest
                        (list class-name))
        do (loop for rule in rules
                 do (etypecase rule
                      (keyword
                         (push class-name
                               (gethash (string-downcase rule)
                                        result)))
                      (cons
                         (let ((prefix (string-downcase (first rule)))
                               (validators (make-validators-from-rule (rest rule))))
                           (push (list* class-name
                                        validators)
                                 (gethash prefix
                                          result))))))
        finally (return result)))



(defun parse-class (string &aux (map (build-classes-map)))
  "Parses CSS class and returns an object."
  ;; Finds prefixes from string end to the start while searching a parser for the prefix.

  (flet ((search-for-class (prefix &optional value)
           (let ((class-name-or-validators
                   (gethash prefix map)))
             (when class-name-or-validators
               (loop for (class-name . validators) in class-name-or-validators
                     do (loop for validator in validators
                              always (funcall validator value)
                              finally (return-from parse-class
                                        class-name)))))))
    (loop with end-pos = (length string)
          with separator-was-found = nil
          for separator-pos = (position #\- string
                                        :from-end t
                                        :end end-pos
                                        :test #'char=)
          when separator-pos
            do (setf separator-was-found
                     t)
               (let* ((prefix (subseq string 0 separator-pos))
                      (value (subseq string (1+ separator-pos))))
                 (search-for-class prefix value)
                 (setf end-pos
                       (1- separator-pos)))
          while (and separator-pos
                     (< 0 separator-pos))
          finally (unless separator-was-found
                    (return (search-for-class string))))))




