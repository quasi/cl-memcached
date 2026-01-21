# Understanding Data Types in cl-memcached

<!-- Generated from: canon/core/foundation/vocabulary.md + CLAUDE.md -->

Learn how cl-memcached handles different data types and encodings for correct cache operations.

## Core Principle

**Memcached stores binary data (octets).** Strings, numbers, and structures must be converted before storage and after retrieval.

## Data Type Rules

### Keys: Always Strings

**Rule:** Keys MUST be strings (1-250 characters, ASCII).

```lisp
;; Correct
(mc-set "user:123" "data")
(mc-set "session-abc" "data")

;; Wrong - will raise cl-mc-error
(mc-set :user-123 "data")      ; Symbol, not string
(mc-set 123 "data")             ; Number, not string
(mc-set '("user" "123") "data") ; List, not string
```

**Why:** Memcached protocol requires text keys. Type safety prevents bugs.

### Values: Octets Internally

**Rule:** Values are stored as octets (unsigned-byte 8 arrays).

**High-level functions handle conversion:**

```lisp
;; mc-set accepts strings
(mc-set "key" "Hello, World!")  ; Automatically converts to octets

;; mc-get-value returns strings
(mc-get-value "key")  ; => "Hello, World!" (string)
```

**Low-level functions require octets:**

```lisp
;; mc-store requires octets
(mc-store "key" (babel:string-to-octets "Hello") :command :set)

;; mc-get+ returns octets
(let ((response (first (mc-get+ (list "key")))))
  (babel:octets-to-string (mc-data response)))  ; => "Hello"
```

### Encoding: UTF-8 by Default

Default encoding is UTF-8, controlled by `*mc-default-encoding*`:

```lisp
;; Default
cl-memcached:*mc-default-encoding*  ; => :utf-8

;; Change if needed
(setf cl-memcached:*mc-default-encoding* :latin1)
```

## Working with Different Data Types

### Strings

**Simplest case** - use high-level functions:

```lisp
;; Store
(mc-set "greeting" "Hello, 世界!")  ; UTF-8 encoding automatic

;; Retrieve
(mc-get-value "greeting")  ; => "Hello, 世界!"
```

**Low-level (manual encoding):**

```lisp
;; Store
(let ((octets (babel:string-to-octets "Hello, 世界!" :encoding :utf-8)))
  (mc-store "greeting" octets :command :set))

;; Retrieve
(let ((response (first (mc-get+ (list "greeting")))))
  (babel:octets-to-string (mc-data response) :encoding :utf-8))
```

### Numbers

**Convert to strings:**

```lisp
;; Store number as string
(mc-set "user-count" (write-to-string 42))

;; Retrieve and parse
(let ((value (mc-get-value "user-count")))
  (parse-integer value))  ; => 42
```

**For counters, use dedicated operations:**

```lisp
;; Initialize
(mc-set "counter" "0")

;; Use atomic operations
(mc-incr "counter")  ; => 1
(mc-incr "counter" 5)  ; => 6
```

### Binary Data

**Use octets directly:**

```lisp
;; Store binary data (e.g., image)
(let ((image-data (read-file-as-octets "photo.jpg")))
  (mc-store "photo:123" image-data :command :set))

;; Retrieve binary data
(let ((response (first (mc-get+ (list "photo:123")))))
  (mc-data response))  ; => #(255 216 255 224 ...)
```

**Don't try to decode binary as text:**

```lisp
;; Wrong - corrupts binary data
(babel:octets-to-string (mc-data response))

;; Right - use octets directly
(write-file-from-octets "photo-copy.jpg" (mc-data response))
```

### JSON Objects

**Serialize as strings:**

```lisp
;; Store JSON
(let* ((user '((:id . 123) (:name . "Alice") (:email . "alice@example.com")))
       (json-string (json:encode-json-to-string user)))
  (mc-set "user:123" json-string))

;; Retrieve and parse JSON
(let* ((json-string (mc-get-value "user:123"))
       (user (json:decode-json-from-string json-string)))
  user)  ; => ((:ID . 123) (:NAME . "Alice") (:EMAIL . "alice@example.com"))
```

### Lists and Arrays

**Serialize with your preferred format:**

```lisp
;; Option 1: JSON
(mc-set "user-ids" (json:encode-json-to-string '(1 2 3 4 5)))

;; Option 2: Lisp format (read/write)
(mc-set "data" (write-to-string '(:a 1 :b 2)))

;; Option 3: Marshal (if available)
(mc-set "complex-data" (marshal:marshal complex-structure))
```

### Structs and Objects

**Serialize before caching:**

```lisp
(defstruct user
  id name email)

;; Option 1: Convert to alist/plist, then JSON
(defun cache-user (user)
  (let ((alist (list (cons :id (user-id user))
                    (cons :name (user-name user))
                    (cons :email (user-email user)))))
    (mc-set (format nil "user:~A" (user-id user))
            (json:encode-json-to-string alist))))

(defun uncache-user (user-id)
  (let* ((json (mc-get-value (format nil "user:~A" user-id)))
         (alist (json:decode-json-from-string json)))
    (make-user :id (cdr (assoc :id alist))
              :name (cdr (assoc :name alist))
              :email (cdr (assoc :email alist)))))
```

## Data Type Reference

| Lisp Type | How to Cache | How to Retrieve |
|-----------|--------------|-----------------|
| String | `(mc-set key string)` | `(mc-get-value key)` |
| Number | `(mc-set key (write-to-string n))` | `(parse-integer (mc-get-value key))` |
| Float | `(mc-set key (write-to-string f))` | `(read-from-string (mc-get-value key))` |
| Boolean | `(mc-set key (if b "t" "nil"))` | `(string= "t" (mc-get-value key))` |
| Symbol | `(mc-set key (symbol-name s))` | `(intern (mc-get-value key))` |
| List | `(mc-set key (json:encode... l))` | `(json:decode... (mc-get-value key))` |
| Binary | `(mc-store key octets :command :set)` | `(mc-data (first (mc-get+ ...)))` |

## Common Patterns

### Pattern 1: Helper Functions for Types

```lisp
(defun cache-integer (key value &key (ttl 0))
  "Cache an integer value."
  (mc-set key (write-to-string value) :timeout ttl))

(defun get-cached-integer (key)
  "Retrieve cached integer."
  (let ((value (mc-get-value key)))
    (when value
      (parse-integer value))))

;; Usage
(cache-integer "count" 42 :ttl 3600)
(get-cached-integer "count")  ; => 42
```

### Pattern 2: Automatic Serialization

```lisp
(defun cache-object (key object &key (ttl 0))
  "Cache any object using JSON serialization."
  (mc-set key (json:encode-json-to-string object) :timeout ttl))

(defun get-cached-object (key)
  "Retrieve cached object."
  (let ((json (mc-get-value key)))
    (when json
      (json:decode-json-from-string json))))

;; Usage
(cache-object "user:123" '((:id . 123) (:name . "Alice")) :ttl 3600)
(get-cached-object "user:123")
```

### Pattern 3: Type-Specific Caches

```lisp
(defclass typed-cache ()
  ((prefix :initarg :prefix :reader cache-prefix)))

(defmethod cache-put ((cache typed-cache) key value &key (ttl 0))
  (let ((full-key (format nil "~A:~A" (cache-prefix cache) key)))
    (cache-object full-key value :ttl ttl)))

(defmethod cache-get ((cache typed-cache) key)
  (let ((full-key (format nil "~A:~A" (cache-prefix cache) key)))
    (get-cached-object full-key)))

;; Usage
(defvar *user-cache* (make-instance 'typed-cache :prefix "user"))
(cache-put *user-cache* 123 '((:name . "Alice")) :ttl 3600)
(cache-get *user-cache* 123)
```

## Encoding Gotchas

### Problem 1: Unicode Characters

```lisp
;; Works - UTF-8 default
(mc-set "greeting" "Hello, 世界!")
(mc-get-value "greeting")  ; => "Hello, 世界!"

;; Breaks with wrong encoding
(setf *mc-default-encoding* :ascii)
(mc-set "greeting" "Hello, 世界!")  ; Error or corruption
```

**Solution:** Use UTF-8 for international text.

### Problem 2: Null Bytes

```lisp
;; Breaks - null bytes in key
(mc-set (format nil "key~Cwith~Cnull" #\Null #\Null) "value")
;; Memcached protocol error

;; Works - escape nulls
(mc-set "key-with-null-escaped" "value")
```

**Solution:** Avoid null bytes in keys.

### Problem 3: Binary/Text Confusion

```lisp
;; Wrong - decoding binary as UTF-8
(let ((binary-data #(255 254 253)))
  (mc-store "key" binary-data :command :set))

(babel:octets-to-string (mc-data (first (mc-get+ (list "key")))))
;; Error or corruption

;; Right - use binary data as-is
(mc-data (first (mc-get+ (list "key"))))
;; => #(255 254 253)
```

## Memory Limits

**Maximum value size:** ~1MB

```lisp
;; Check before caching large data
(defun cache-if-small-enough (key data &key (max-size (* 1024 1024)))
  (let ((octets (babel:string-to-octets data)))
    (if (< (length octets) max-size)
        (mc-set key data)
        (warn "Data too large to cache: ~A bytes" (length octets)))))
```

**For larger data:**
- Split into chunks
- Compress before caching
- Use database instead

## Verification

Test data type handling:

```lisp
(defun test-data-types ()
  "Verify data types work correctly."
  ;; String
  (mc-set "test-string" "Hello, World!")
  (assert (string= "Hello, World!" (mc-get-value "test-string")))

  ;; Number
  (mc-set "test-number" (write-to-string 42))
  (assert (= 42 (parse-integer (mc-get-value "test-number"))))

  ;; Unicode
  (mc-set "test-unicode" "Hello, 世界!")
  (assert (string= "Hello, 世界!" (mc-get-value "test-unicode")))

  ;; Binary
  (let ((binary #(1 2 3 4 5)))
    (mc-store "test-binary" binary :command :set)
    (assert (equalp binary (mc-data (first (mc-get+ (list "test-binary")))))))

  ;; JSON
  (let ((data '((:id . 123) (:name . "Alice"))))
    (mc-set "test-json" (json:encode-json-to-string data))
    (assert (equal data
                  (json:decode-json-from-string (mc-get-value "test-json")))))

  (format t "Data type tests passed!~%"))

(test-data-types)
```

## Recap

You've learned:

✓ Keys are strings, values are octets
✓ High-level vs low-level functions
✓ UTF-8 encoding by default
✓ How to cache different Lisp types
✓ Serialization patterns for complex data
✓ Common encoding gotchas

## What's Next

- [API Reference](../reference/api-reference.md) - Complete function signatures
- [Tutorials](../tutorials/) - Practical examples
- [Core Concepts](core-concepts.md) - Broader understanding

---

**See also:** [Type Constraints](../../CLAUDE.md#3-type-constraints) (Implementation specification)
