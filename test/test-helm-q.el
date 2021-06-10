;;; -*- lexical-binding: t; -*-

;;; Tests for `helm-q', using the Buttercup framework
;;;
;;; Run either with `buttercup-run-at-point' when the cursor is inside a test suite (`describe'),
;;; or with the command-line `buttercup` script (the latter being the primary approach for the framework).


(require 'buttercup)
(require 'json)

(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name (buffer-file-name))))))
(require 'helm-q)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions


;;; We emulate the action of some functions from helm-source.el, to avoid depending on them
;;; and mixing them in with the testing
;;; (though perhaps that should be done, so the sources are tested end-to-end?)
(defun test-helm-q/make-source (name class &rest args)
  "Creates an associative list from a new instance of the source class, like `helm-make-source' does

However this function omits some setup of the fields on the instance that `helm-make-source' performs.

NAME is a string with a name for the source.
CLASS is an eieio class object.
ARGS are keyword value pairs as defined in CLASS (passed to the constructor).

Returns the associative list made from the new source instance."
  (let ((source-object (apply #'make-instance class name args)))
    (cl-loop for slot-name in (object-slots source-object)
             for slot-val = (slot-value source-object slot-name)
             when slot-val
             collect (cons slot-name (unless (eq t slot-val) slot-val)))))


(defun test-helm-q/make-attr-getter (source-list)
  "Creates a stub function to replace `helm-attr' for a particular source

SOURCE-LIST: an associative list, the source of attributes for the getter function."
  (lambda (attr-name &optional source compute)
    "Get the value of attribute ATTR-NAME of SOURCE"
    ;; XXX need to handle `setf', i.e. declare how to use the function calls as 'generalized variables' —
    ;; but can that be done with overridden functions and lambdas?
    ;; see `(declare (gv-setter))` in `helm-attr'
    (if source (error "Can't handle the source argument: %s" source))
    (if compute (error "Can't handle the 'compute' argument"))
    (cdr (assq attr-name source-list))))


(defun test-helm-q/make-attr-setter (source-list)
  "Creates a stub function to replace `helm-attrset' for a particular source

SOURCE-LIST: an associative list, on which the attributes will be set by the function."
  (lambda (attr-name value &optional source)
    "Set the value of ATTR-NAME of source SOURCE to VALUE

If ATTR-NAME doesn't exists in source it is created with value VALUE.

If operation succeeds, return VALUE, otherwise nil."
    (if source (error "Can't handle the source argument: %s" source))
    (if-let ((attr-cons (assq attr-name source-list)))
        (setcdr attr-cons value)
      (progn
        (setcdr source-list (cons (cons attr-name value)
                                  (cdr source-list)))
        value))))


(defmacro test-helm-q/with-temp-instances-files (files-content &rest body)
  "Sets up temporary files with the KDB instances' JSON data for helm-q, and runs remaining code

Removes the files afterwards, even if an error occurs in the code.

FILES-CONTENT: a list of files to create, each being a list of form (FILE-NAME FILE-CONTENT).
The content is raw text to write in the file."
  (declare (indent 1))
  `(let ((helm-q-config-directory (make-temp-file "helm-q-" t)))
     (dolist (file ,files-content)
       (with-temp-file (concat helm-q-config-directory "/" (car file))
         (insert (cadr file))))
     (unwind-protect
         (progn ,@body)
       (delete-directory helm-q-config-directory t))))


(defmacro test-helm-q/with-temp-instances-text (json-content &rest body)
  "Sets up a temporary instances JSON file with the data for helm-q, and runs remaining code

Raw encoded data is expected as the argument, as a string.

Removes the file afterwards, even if an error occurs in the code.

JSON-CONTENT: the raw string to put in the file"
  (declare (indent 1))
  `(test-helm-q/with-temp-instances-files (list (list "instances.json" ,json-content))
    ,@body))


(defmacro test-helm-q/with-temp-instances (instances-data &rest body)
  "Sets up a temporary instances JSON file with the data for helm-q, and runs remaining code

The data is encoded as JSON, to be written in the file.

Removes the file afterwards, even if an error occurs in the code.

INSTANCES-DATA: structured data to encode as JSON and put in the file"
  (declare (indent 1))
  `(test-helm-q/with-temp-instances-text (json-encode ,instances-data) ,@body))


;; XXX Change the name so it's not confused with KDB instances?
;; E.g. to something like `with-source-instance'.
;; Likewise, might want to rename the arguments so they're more distinct.
(defmacro test-helm-q/with-instance (instance-var instances-data &rest body)
  "Creates a helm-q source with specified instance data INSTANCE-DATA, binds it to INSTANCE-VAR, and runs specified code"
  (declare (indent 2))
  `(test-helm-q/with-temp-instances ,instances-data
     (let ((,instance-var (test-helm-q/make-source "helm-q" 'helm-q-source)))
       (spy-on 'helm-attr :and-call-fake (test-helm-q/make-attr-getter ,instance-var))
       (spy-on 'helm-attrset :and-call-fake (test-helm-q/make-attr-setter ,instance-var))
       (helm-q-source-list--init)

       ,@body)))


(defun test-helm-q/mock-instance-data (&optional override)
  "Creates data for one mock KDB instance for the tests (an associated list), plus updates some fields if provided

OVERRIDE: an associated list with key-value pairs to override in the returned instance data

Returns an associated list with the mock instance data"
  (test-helm-q/merge-alists
   '((address . "host.domain.com:5000")
     (env . "prod")
     (region . "nam")
     (service . "TAQ.HDB")
     (tablescolumns
      (trade . ["price" "size"])
      ;; 'quote' here is the name of a table, not the elisp function
      ;; (XXX might want to change the 'quote' name in tests, to avoid confusion.)
      (quote . ["bid" "ask" "bsize" "asize"]))
     (functions (upd . "{sum 1 2}")
                (endofday . "{avg 3 4}"))
     (variables . ["foo" "bar"]))
   override))


(defun test-helm-q/mock-instances-data (&rest overrides)
  "Creates a vector with mock KDB instances data

Generates one instance by default. If OVERRIDES are specified, generates the number of instances equal to the number of supplied arguments.

OVERRIDES: associated lists with key-value pairs to override in the returned instances data

Returns a vector with the mock instances data"
  (apply #'vector (mapcar #'test-helm-q/mock-instance-data
                          (if (null overrides) '(nil) overrides))))


(defun test-helm-q/merge-alists (&rest alists)
  "Merges associated lists into one, overwriting values in earlier ones with later ones

ALISTS: associated lists to merge

Returns the resulting list"
  ;; copying the first list to prevent it being modified by updates to the function's return value
  (let ((result (copy-alist (car alists))))
    (dolist (alist (cdr alists))
      (dolist (key-value alist)
        (if-let ((cell (assoc (car key-value) result)))
            ;; update the existing key-value pair
            (setcdr cell (cdr key-value))
          ;; append the new key-value cons — by creating a new cons to prevent undesired updates by reference
          ;; (though not sure that copying is strictly necessary)
          (setq result (append result (list (cons (car key-value) (cdr key-value))))))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests proper


(describe "helm-q-source-list--init"

  ;; XXX should test with more than one item in the list
  (it "populates the list from a json file in the preconfigured directory"
    ;; Testing with raw text, to make sure that the code in question decodes what we give it here.
    (test-helm-q/with-temp-instances-text "[{\"address\":\"host.domain.com:5000\", \"env\":\"prod\", \"region\":\"nam\", \"service\":\"TAQ.HDB\", \"tablescolumns\":{\"trade\":[\"price\", \"size\"], \"quote\":[\"bid\", \"ask\", \"bsize\", \"asize\"]}, \"functions\":{\"upd\":\"{sum 1 2}\", \"endofday\":\"{avg 3 4}\"}, \"variables\":[\"foo\", \"bar\"]}]"
      (let ((source-instance (test-helm-q/make-source "helm-q" 'helm-q-source)))
        (spy-on 'helm-attr :and-call-fake (test-helm-q/make-attr-getter source-instance))
        (spy-on 'helm-attrset :and-call-fake (test-helm-q/make-attr-setter source-instance))
        (helm-q-source-list--init)

        ;; XXX The order of the conses might change since it comes from a JSON object? should sort the list before comparing?
        ;; Or use Buttercup's ':to-have-same-items-as' matcher
        (expect (assoc-default 'candidates source-instance)
                :to-equal
                '((
                   ;; This structure is 'actually' a cons, but may be displayed as a merged list since it's equivalent to that.
                   #("host.domain.com:5000  TAQ.HDB  prod  nam" 0 40 (face bold instance ((address . "host.domain.com:5000") (env . "prod") (region . "nam") (service . "TAQ.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host.domain.com:5000")
                    (env . "prod")
                    (region . "nam")
                    (service . "TAQ.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   ))
                ))))

  (it "joins lists from multiple json files"
    ;; Testing with raw text, to make sure that the code in question decodes what we give it here.
    (test-helm-q/with-temp-instances-files
        '(("instances1.json" "[{\"address\":\"host.domain.com:5000\", \"env\":\"prod\", \"region\":\"nam\", \"service\":\"TAQ.HDB\", \"tablescolumns\":{\"trade\":[\"price\", \"size\"], \"quote\":[\"bid\", \"ask\", \"bsize\", \"asize\"]}, \"functions\":{\"upd\":\"{sum 1 2}\", \"endofday\":\"{avg 3 4}\"}, \"variables\":[\"foo\", \"bar\"]}]")
          ("instances2.json" "[{\"address\":\"host2.domain.com:5000\", \"env\":\"prod\", \"region\":\"nam\", \"service\":\"TAQ2.HDB\", \"tablescolumns\":{\"trade\":[\"price\", \"size\"], \"quote\":[\"bid\", \"ask\", \"bsize\", \"asize\"]}, \"functions\":{\"upd\":\"{sum 1 2}\", \"endofday\":\"{avg 3 4}\"}, \"variables\":[\"foo\", \"bar\"]}]"))
      (let ((source-instance (test-helm-q/make-source "helm-q" 'helm-q-source)))
        (spy-on 'helm-attr :and-call-fake (test-helm-q/make-attr-getter source-instance))
        (spy-on 'helm-attrset :and-call-fake (test-helm-q/make-attr-setter source-instance))
        (helm-q-source-list--init)

        (expect (assoc-default 'candidates source-instance)
                :to-equal
                '((
                   #("host.domain.com:5000   TAQ.HDB   prod  nam" 0 40 (face bold instance ((address . "host.domain.com:5000") (env . "prod") (region . "nam") (service . "TAQ.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host.domain.com:5000")
                    (env . "prod")
                    (region . "nam")
                    (service . "TAQ.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   )
                  (
                   #("host2.domain.com:5000  TAQ2.HDB  prod  nam" 0 40 (face bold instance ((address . "host2.domain.com:5000") (env . "prod") (region . "nam") (service . "TAQ2.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host2.domain.com:5000")
                    (env . "prod")
                    (region . "nam")
                    (service . "TAQ2.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   ))
                ))))

  (it "aligns displayed instance parameters to the longest one among several instances"
    (test-helm-q/with-instance source-instance
        (test-helm-q/mock-instances-data '((address . "host.extra-long-domain-name-like-seriously-long.com:65535"))
                                         '((env . "super-env-name-turbo-extra-special-champion-edition")))
      (expect (assoc-default 'candidates source-instance)
                :to-equal
                '((
                   #("host.extra-long-domain-name-like-seriously-long.com:65535  TAQ.HDB  prod                                                 nam" 0 124 (face bold instance ((address . "host.extra-long-domain-name-like-seriously-long.com:65535") (env . "prod") (region . "nam") (service . "TAQ.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host.extra-long-domain-name-like-seriously-long.com:65535")
                    (env . "prod")
                    (region . "nam")
                    (service . "TAQ.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   )
                  (
                   #("host.domain.com:5000                                       TAQ.HDB  super-env-name-turbo-extra-special-champion-edition  nam" 0 124 (face bold instance ((address . "host.domain.com:5000") (env . "super-env-name-turbo-extra-special-champion-edition") (region . "nam") (service . "TAQ.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host.domain.com:5000")
                    (env . "super-env-name-turbo-extra-special-champion-edition")
                    (region . "nam")
                    (service . "TAQ.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   ))
                )))

  (it "uses only preconfigured columns for instances' display strings"
    (test-helm-q/with-temp-instances (test-helm-q/mock-instances-data)
      (let ((source-instance (test-helm-q/make-source "helm-q" 'helm-q-source)))
        (spy-on 'helm-attr :and-call-fake (test-helm-q/make-attr-getter source-instance))
        (spy-on 'helm-attrset :and-call-fake (test-helm-q/make-attr-setter source-instance))

        (setcdr (assoc 'candidate-columns source-instance) '(address region))
        (helm-q-source-list--init)

        (expect (assoc-default 'candidates source-instance)
                :to-equal
                '((
                   #("host.domain.com:5000  nam" 0 25 (face bold instance ((address . "host.domain.com:5000") (env . "prod") (region . "nam") (service . "TAQ.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host.domain.com:5000")
                    (env . "prod")
                    (region . "nam")
                    (service . "TAQ.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   ))
                ))))

  (it "uses Helm's buffers column separator var to join columns for the display string"
    (test-helm-q/with-temp-instances (test-helm-q/mock-instances-data)
      (let ((source-instance (test-helm-q/make-source "helm-q" 'helm-q-source)))
        (spy-on 'helm-attr :and-call-fake (test-helm-q/make-attr-getter source-instance))
        (spy-on 'helm-attrset :and-call-fake (test-helm-q/make-attr-setter source-instance))

        (let ((helm-buffers-column-separator "//"))
          (helm-q-source-list--init))

        (expect (assoc-default 'candidates source-instance)
                :to-equal
                '((
                   #("host.domain.com:5000//TAQ.HDB//prod//nam" 0 40 (face bold instance ((address . "host.domain.com:5000") (env . "prod") (region . "nam") (service . "TAQ.HDB") (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"])) (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}")) (variables . ["foo" "bar"]))))
                   .
                   ((address . "host.domain.com:5000")
                    (env . "prod")
                    (region . "nam")
                    (service . "TAQ.HDB")
                    (tablescolumns (trade . ["price" "size"]) (quote . ["bid" "ask" "bsize" "asize"]))
                    (functions (upd . "{sum 1 2}") (endofday . "{avg 3 4}"))
                    (variables . ["foo" "bar"]))
                   ))
                ))))
  )


(defun remove-keyword-params (seq keyword-params-list)
  "Cleans up specified keyword params KEYWORD-PARAMS-LIST from the sequence SEQ"
  (if (null seq) nil
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (memq head keyword-params-list) (remove-keyword-params (cdr tail) keyword-params-list)
        (cons head (remove-keyword-params tail keyword-params-list))))))


(cl-defmacro test-helm-q/run-the-match-function-and-expect (pattern &rest expectations &key mock-data candidate-string &allow-other-keys)
  "Performs a test on `helm-q-source-match-function' with the specified pattern and expectations

Note that the keyword arguments MOCK-DATA and CANDIDATE-STRING must be passed *before* the rest-arguments EXPECTATIONS.

PATTERN: Helm pattern to match on

EXPECTATIONS: a variable number of arguments to pass to `expect' as the behavior to check. These must be listed at the end of the argument list — after keyword arguments if those are supplied.

Keyword arguments:

MOCK-DATA: field values to override in the mock KDB instances data — as a list of associative lists for each mock instance.
The number of instances will be equal to the number of items in the list, or one item if nothing is supplied
(see `test-helm-q/mock-instances-data' for details).

CANDIDATE-STRING: the display string to pass to `helm-q-source-match-function'"
  (let ((expectations (remove-keyword-params expectations '(:mock-data :candidate-string)))
        ;; injecting the candidate-string as a constant value so Buttercup's error message have it nicely
        ;; embedded, instead of the whole expression verbatim
        (candidate-string (or candidate-string
                               ;; display-string for default mock data
                               "host.domain.com:5000  TAQ.HDB  prod  nam")))
    `(test-helm-q/with-instance source-instance (apply #'test-helm-q/mock-instances-data ,mock-data)
       (let ((helm-pattern ,pattern))
         (expect (helm-q-source-match-function ,candidate-string)
                 ,@expectations)))))


(describe "helm-q-source-match-function"

  ;; XXX should throw a clear error instead of an arbitrary one
  (it "throws an error if called with a string that doesn't match a candidate's display string"
    (test-helm-q/with-instance source-instance (test-helm-q/mock-instances-data)
      (let ((helm-pattern "The quick brown fox jumps over the lazy dog"))
        (expect (helm-q-source-match-function "no such thing")
                :to-throw 'wrong-type-argument))))

  ;; XXX should throw a clear error instead of passing 'nil' to `helm-buffer--match-pattern'
  (it "throws an error if any of defined columns isn't found in the candidate"
    (test-helm-q/with-instance source-instance (test-helm-q/mock-instances-data)
      (setcdr (assoc 'candidate-columns source-instance) '(address service env region cowabunga))
      (let ((helm-pattern "The quick brown fox jumps over the lazy dog"))
        (expect (helm-q-source-match-function "host.domain.com:5000  TAQ.HDB  prod  nam")
                :to-throw 'wrong-type-argument))))

  (it "returns nil if no columns match"
    (test-helm-q/run-the-match-function-and-expect "The quick brown fox jumps over the lazy dog" :to-be nil))

  (it "returns non-nil if any column matches on the whole pattern"
    (test-helm-q/run-the-match-function-and-expect "taq" :not :to-be nil))

  (it "returns non-nil if a substring of any column matches on the pattern"
    (test-helm-q/run-the-match-function-and-expect "omai" :not :to-be nil))

  (it "ignores text case when matching"
    (test-helm-q/run-the-match-function-and-expect "host"
                                                   :candidate-string "hOsT.dOmaIn.cOm  TAQ.HDB  prod  nam"
                                                   :mock-data '(((address . "hOsT.dOmaIn.cOm")))
                                                   :not :to-be nil))

  (it "ignores pattern case when matching"
    (test-helm-q/run-the-match-function-and-expect "DomAIn" :not :to-be nil))

  (it "doesn't match words that include punctuation characters"
    (test-helm-q/run-the-match-function-and-expect "taq!" :to-be nil))

  ;; `helm-buffer--match-pattern' doesn't allow multiple words, matching non-alphanumerics literally instead.
  ;; Multiple-word matching is done by `helm-mm-match' (added to the list of match functions by Helm
  ;; since the 'multimatch' option is specified on the source.
  (it "returns nil if multiple words are used in the pattern even when they match a column"
    (test-helm-q/run-the-match-function-and-expect "taq hdb" :to-be nil)

    ;; let's try special characters for good measure
    (test-helm-q/run-the-match-function-and-expect "taq%hdb" :to-be nil))

  (it "returns non-nil if multiple words are used in the pattern and match a column exactly, including word delimiters"
    (test-helm-q/run-the-match-function-and-expect "taq.hdb" :not :to-be nil)

    ;; again, gonna check with special characters just in case
    (test-helm-q/run-the-match-function-and-expect "taq%hdb"
                                                   :mock-data '(((service . "TAQ%HDB")))
                                                   :candidate-string "host.domain.com:5000  TAQ%HDB  prod  nam"
                                                   :not :to-be nil))

  (it "returns nil if different columns match different words of the pattern"
    (test-helm-q/run-the-match-function-and-expect "taq prod" :to-be nil))

  ;; XXX rename the test to highlight the difference with full-on fuzzy matching by helm-mm-match?
  (it "returns non-nil if any column matches fuzzily"
    (test-helm-q/run-the-match-function-and-expect "adrswtspcs"
                                                   :mock-data '(((address . "address with spaces")))
                                                   :candidate-string "address with spaces  TAQ.HDB  prod  nam"
                                                   :not :to-be nil)

    ;; gonna check with spaces too
    (test-helm-q/run-the-match-function-and-expect "adrs wt spcs"
                                                   :mock-data '(((address . "address with spaces")))
                                                   :candidate-string "address with spaces  TAQ.HDB  prod  nam"
                                                   :not :to-be nil))

  (it "doesn't match on a pattern that is broken up with a space"
    (test-helm-q/run-the-match-function-and-expect "dom in" :to-be nil))

  (it "returns non-nil if a table name for the instance matches any word in the search pattern"
    ;; first check that there's no match if the table name is not in the pattern
    (test-helm-q/run-the-match-function-and-expect "good today" :to-be nil)
    (test-helm-q/run-the-match-function-and-expect "good trade today" :not :to-be nil))

  (it "returns non-nil if a column name in any of the instance's tables matches any word in the search pattern"
    ;; check that there's no match if the column name is not in the pattern
    (test-helm-q/run-the-match-function-and-expect "extra low stocks" :to-be nil)
    (test-helm-q/run-the-match-function-and-expect "extra low price stocks" :not :to-be nil))

  (it "returns non-nil if a function name for the instance matches any word in the search pattern"
    ;; check that there's no match if the function name is not in the pattern
    (test-helm-q/run-the-match-function-and-expect "nice function" :to-be nil)
    (test-helm-q/run-the-match-function-and-expect "endofday nice function" :not :to-be nil))

  (it "returns non-nil if a variable name for the instance matches any word in the search pattern"
    ;; check that there's no match if the variable name is not in the pattern
    (test-helm-q/run-the-match-function-and-expect "close exchange open" :to-be nil)
    (test-helm-q/run-the-match-function-and-expect "close exchange open bar" :not :to-be nil))

  ;; XXX rename the test to highlight the difference with full-on fuzzy matching by helm-mm-match?
  (it "returns non-nil if an extra column matches fuzzily"
    (test-helm-q/run-the-match-function-and-expect "vrblwtspcs"
                                                   :mock-data '(((variables . ["variable with spaces"])))
                                                   :not :to-be nil)
    ;; gonna check with spaces too
    (test-helm-q/run-the-match-function-and-expect "vrbl wt spcs"
                                                   :mock-data '(((variables . ["variable with spaces"])))
                                                   :not :to-be nil))

  (it "matches extra columns even if extra spaces are used in the pattern"
    (test-helm-q/run-the-match-function-and-expect "vrbl wt spcs"
                                                   :mock-data '(((variables . ["variablewithoutspaces"])))
                                                   :not :to-be nil))

  (it "returns non-nil if both main columns and extra columns match"
    ;; check that there's no match if the fields are not in the pattern
    (test-helm-q/run-the-match-function-and-expect "for" :to-be nil)
    (test-helm-q/run-the-match-function-and-expect "bid for domain" :not :to-be nil))

  (it "doesn't match extra field on words that include punctuation characters"
    (test-helm-q/run-the-match-function-and-expect "price!" :to-be nil))

  )


(describe "helm-q-source-filtered-candidate-transformer"

  ;; XXX Perhaps should exclude the properties from expected results and match only string contents?
  ;; At least ignore the 'instance' properties and check only 'bold'.
  ;; Alternatively, check the properties thoroughly, including the ranges ('0 40' etc. — currently they don't have to match).

  ;; XXX should test with more than one item in the list
  (it "returns pairs of display string and instance data for instances in the argument; omits the second row if there is no current search pattern"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data)))
      (test-helm-q/with-instance source-instance (vector instance-data)
        ;; the pair of the display string and instance data comes from the candidates list, as populated by `helm-q-source-list--init'
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam" 0 40 (face bold instance ,instance-data))
                             .
                             ,instance-data))))))

  (it "ignores the display-string in the input, recreating it anew in the output"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("foo-bar-quux" . ,instance-data)))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam" 0 40 (face bold instance ,instance-data))
                             .
                             ,instance-data))))))

  (it "ignores the candidates field of the source"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data)))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (setcdr (assoc 'candidates source-instance) '("random data that won't be used"))
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam" 0 40 (face bold instance ,instance-data))
                             .
                             ,instance-data))))))

  (it "includes the table name in the second row if a table name for the instance matches any word in the search pattern"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data))
           (helm-pattern "good trade today"))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam\n    Table:'trade'" 0 40 (face bold instance ,instance-data) 40 58 (instance ,instance-data)) . ,instance-data))
                ))))

  (it "includes the column name in the second row if a column name for the instance matches any word in the search pattern"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data))
           (helm-pattern "nice price"))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam\n    Column:'trade.price'" 0 40 (face bold instance ,instance-data) 40 65 (instance ,instance-data)) . ,instance-data))
                ))))

  (it "includes the function name in the second row if a function name for the instance matches any word in the search pattern"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data))
           (helm-pattern "endofday"))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam\n    Function:'endofday'" 0 40 (face bold instance ,instance-data) 40 64 (instance ,instance-data)) . ,instance-data))
                ))))

  (it "includes the variable name in the second row if a variable name for the instance matches any word in the search pattern"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data))
           (helm-pattern "close exchange, open bar"))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam\n    Var:'bar'" 0 40 (face bold instance ,instance-data) 40 54 (instance ,instance-data)) . ,instance-data))
                ))))

  (it "includes multiple fields in the second row when they match any word in the search pattern"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data))
           ;; the columns are deliberately not matched, to check that they're excluded from the result
           (helm-pattern "good trade at endofday close exchange open bar"))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000  TAQ.HDB  prod  nam\n    Table:'trade'  Function:'endofday'  Var:'bar'" 0 40 (face bold instance ,instance-data) 40 90 (instance ,instance-data)) . ,instance-data))
                ))))

  (it "uses Helm's buffers column separator var to indent and separate the extra fields in the second row"
    (let* ((instance-data (test-helm-q/mock-instance-data))
           (instance-candidate `("host.domain.com:5000  TAQ.HDB  prod  nam" . ,instance-data))
           ;; the columns are deliberately not matched, to check that they're excluded from the result
           (helm-pattern "good trade at endofday close exchange open bar")
           (helm-buffers-column-separator "//"))
      (test-helm-q/with-instance source-instance (vector instance-data)
        (expect (helm-q-source-filtered-candidate-transformer (list instance-candidate) source-instance)
                :to-equal `((#("host.domain.com:5000//TAQ.HDB//prod//nam\n////Table:'trade'//Function:'endofday'//Var:'bar'" 0 40 (face bold instance ,instance-data) 40 90 (instance ,instance-data)) . ,instance-data))
                ))))

  )
