(define-library (sys)
  ;; library to be populated by the org.jelly.app.App before running the app
  ;; (this could probably be done in the JellyRuntime constructor)
  (export argv)
  (begin
    (define argv null)))

