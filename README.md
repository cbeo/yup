# Yup, its another static site builder

Yup is a small system for doing interactive development of static
sites in common lisp.

##  A wee example

Lets say you want to make a gallery site, and that you have a
directory on you laptop that looks like this:

``` 
$ tree 

├── galleries
│   └── park-photos
│       ├── published
│       │   └── photo1.png
│       │   └── photo2.png
│       │   └── photo3.png
│       │   └── photo4.png
│       │   └── photo5.png
│       └── unpublished
│       │   └── photo6.png
│       │   └── photo7.png
│       │   └── photo8.png
│       │   └── photo9.png
│   └── grand-canyon
│       ├── published
│       │   └── photo10.png
│       │   └── photo20.png
│       │   └── photo30.png
│       │   └── photo40.png
│       └── unpublished
│       │   └── photo60.png
│       │   └── photo80.png



``` 

You'll want the following:

- a page that lists galleries that shows a thumbnail from one of the published photos
- a page for each gallery that shows thumbnails of the photos
- a bit of JS that implements a slide show application to view photos with
- and some css to style it all.

Here is a *sketch* of how you'd do this with `yup`.

### Define Some Pages

``` lisp

;; defines a function PAGE/MAIN
(defpage main (:styles ("/css/main.css"))
  (:h1 "My Photos")
  (:div :class "gallery-list"
    (dolist (gallery-link (get-gallery-links))
        (view/gallery-entry gallery-link))))
        
;; defines a function VIEW/GALLERY-ENTRY 
;; to be called from within functions defined usign defpage
(defview gallery-entry (gallery-link)
  (:div :class "card"
    (:a :href gallery-link 
        (:img :class "gallery-preview" 
              :src (get-first-image-in-gallery gallery-link)))))
        
;; defines PAGE/GALLERY
(defpage gallery (:styles ("/css/main.css") 
                  :js ("/js/slideshow.js")) 
         (gallery-link)
  (div :class "gallery"
    (dolist (img-link (images-in-gallery gallery-link))
      (yup:view img-link)))) ; note here we're using the default view for img-link asset

      
(defview gallery-card (img-link) 
   (:div :class "card"
         (:img :src img-link)))

``` 

The `defpage` and `defview` macros define functions.  Functions
defined with `defview` are meant to be called form within the body of
functions defined with `defpage`.  

An *artifact* is a document built by `yup`. Calling a function defined
with `defpage` will add an artifact to a site.  You'll see how this
works below we you define the `recipe` function.

### Define Stylesheets and Scripts

The above pages make reference to some stylesheets and javascript
files. You can supply these yourself, or you can add them using
`defscript` and `defstyle` macros:


``` lisp
;; a stylesheet  using LASS syntax
(defstyle main 
   ((bg "#FFF") 
    (color "#000") 
    (secondary-bg "#eee") 
    (second-color "#444"))

  (body 
    :background-color #(bg)
    :color #(color)
    :margin 0
    :padding 0)
    
  (.card 
   :color #(secondary-color)
   :background-color #(secondary-bg)
   :border-radius 4px))
   
;; a script that theoretically implements a slideshow feature
(defscript slideshow (gallery) 
    (defun start-slideshow-with (card) ... )

   (defun on-image-click (event) ... )
   
   (dolist (card (ps:chain document (get-elements-by-class "card")))
      (ps:chain card (add-event-listener "click" on-image-click))))

``` 

Just like `defpage`, `defscript` and `defstyle` also define functions
that are used to add artifacts to sites.  

### You'll surely need some helpers

And here are some stubs of hypothetical helpers used above

``` lisp
;; hypothetical helpers

(defun images-in-gallery (gallery-link)
   "returns a list of links to images in the gallery associated with GALLERY-LINK"
   ....)

(defun get-first-image-in-gallery (gallery-link)
  "returns a the first image in a gallery to use as a frontspiece"
   ....)
   
(defun get-gallery-links () 
  "returns a list of links to gallery pages"
   ....)
   
```


### Make a site construction recipe

The real action happens when you define a site construction recipe.  A
recipe should accept zero arguments and be called within a context
where `yup:*site*` is bound to a site instance. See below.

``` lisp

(defun recipe ()

  ;; ADD-DIRECTORY-ASSETS lets you add a bunch of assets to a site in one swoop,
  ;; it takes a directory and a default view function which can be used to 
  ;; embedd those assets into pages using the YUP:VIEW function.
  (yup:add-directory-assets 
    "~/path/to/some/images"
    'view/gallery-card
    :pattern "published/.*png$")  ; regex pattern to filter out files 
    
  ;; now you can add some pages.

  (page/main "index.html" :title "My Photos")

  ;; and one page per gallery
  (dolist (page-link (get-gallery-links))
    (page/gallery page-link :title (gallery-title-from-link page-link)))

  ;; don't forget to add the styles and scripts
  (style/main "/css/main.css"  :bg "#123" 
                               :color "#456" 
                               :secondary-bg "#789" 
                               :secondary-color "#ABC") 
  (script/slideshow "/js/slideshow.js"))

```

Finally, to actually build your site you need to define a `yup:site`
instance and call the `yup:build` function.

```lisp 

(defvar *my-site* 
    (yup:make-site "My Galleries" :build-to "/path/to/docroot/www"))

(let ((yup:*site* *my-site*)) 
     ;; most of the functions in recipe assume yup:*site* is bound
   (recipe))
   
(yup:build *my-site*)

``` 

### But you really want to do interactive development

To interactively develop your site, pass your site and recipe to `yup:hack-on`

```lisp 

(yup:hack-on *my-site* 'recipe :port 4242 :auto-refresh t :log-to-repl nil)

``` 

Now you can point your browser to http:localhost:4242 and start
hacking!  Whenever you recompile any of your `defpage`, `defstyle`,
`defview`, `defscript` forms, or whenever you recompile your build
recipe function, or add assets to your site, the page will refresh
with the updates.
