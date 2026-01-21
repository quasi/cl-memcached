# How-To: Use CAS for Safe Concurrent Updates

<!-- Generated from: canon/features/storage/scenarios/cas-operation.md -->

Safely update cached data in concurrent environments using CAS (Check-And-Set) operations.

## Problem

You need to update cached data that might be modified by concurrent processes:

- Updating user profiles
- Modifying complex data structures
- Implementing distributed algorithms
- Preventing lost updates

Simple read-modify-write creates race conditions.

## Solution

Use CAS (Check-And-Set) for optimistic concurrency control:

1. Read data WITH CAS token
2. Modify locally
3. Write back ONLY if CAS token matches
4. Retry if concurrent modification detected

**No locks needed.** Server guarantees atomicity.

## Prerequisites

- Memcached server running
- cl-memcached loaded
- Understanding of basic operations ([see Tutorial](../tutorials/01-basics.md))

## Quick Example

```lisp
;; Read with CAS token
(let* ((response (first (cl-memcached:mc-gets+ (list "user:profile"))))
       (cas-token (cl-memcached:mc-cas-unique response))
       (current-data (babel:octets-to-string (cl-memcached:mc-data response))))

  ;; Modify locally
  (let ((new-data (update-profile current-data)))

    ;; Write back with CAS
    (let ((result (cl-memcached:mc-cas "user:profile" new-data cas-token)))
      (cond
        ((string= result "STORED")
         (format t "Update successful!~%"))
        ((string= result "EXISTS")
         (format t "Conflict detected - retry needed~%"))
        ((string= result "NOT_FOUND")
         (format t "Key was deleted~%"))))))
```

## How It Works

### Without CAS (RACE CONDITION)

```
Thread 1: Read profile (version=42)
Thread 2: Read profile (version=42)
Thread 1: Modify profile locally
Thread 2: Modify profile locally (different changes!)
Thread 1: Write profile back
Thread 2: Write profile back  <- OVERWRITES Thread 1's changes!
```

**Result:** Lost update. Thread 1's changes are lost.

### With CAS (SAFE)

```
Thread 1: GETS profile (CAS token=abc123)
Thread 2: GETS profile (CAS token=abc123)
Thread 1: Modify profile locally
Thread 2: Modify profile locally
Thread 1: CAS with token=abc123 → SUCCESS, new token=def456
Thread 2: CAS with token=abc123 → CONFLICT (token is now def456)
Thread 2: Retry from GETS...
```

**Result:** No lost updates. Conflicts detected and handled.

## Step-by-Step Guide

### Step 1: Read Data with CAS Token

Use `mc-gets+` (not `mc-get+`) to get CAS token:

```lisp
(let* ((responses (cl-memcached:mc-gets+ (list "my-key")))
       (response (first responses)))
  (when response
    (let ((cas-token (cl-memcached:mc-cas-unique response))
          (data (cl-memcached:mc-data response)))
      ;; cas-token is now available
      ...)))
```

**Key differences:**
- `mc-get+` - No CAS token
- `mc-gets+` - Includes CAS token

### Step 2: Modify Data Locally

Work with the data locally (no server interaction):

```lisp
(let* ((old-data (babel:octets-to-string data))
       (new-data (modify-profile old-data)))  ; Your business logic
  ...)
```

### Step 3: Attempt CAS Update

Write back with CAS token:

```lisp
(let ((result (cl-memcached:mc-cas "my-key" new-data cas-token)))
  (cond
    ((string= result "STORED")
     ;; Success!
     t)
    ((string= result "EXISTS")
     ;; Concurrent modification - retry
     nil)
    ((string= result "NOT_FOUND")
     ;; Key deleted
     nil)))
```

**Response codes:**
- `"STORED"` - Update succeeded
- `"EXISTS"` - CAS token mismatch (concurrent modification)
- `"NOT_FOUND"` - Key doesn't exist

### Step 4: Handle Conflicts with Retry Loop

```lisp
(loop
  (let* ((responses (mc-gets+ (list "my-key")))
         (response (first responses)))

    (unless response
      (error "Key not found"))

    (let* ((cas-token (mc-cas-unique response))
           (old-data (babel:octets-to-string (mc-data response)))
           (new-data (modify-profile old-data))
           (result (mc-cas "my-key" new-data cas-token)))

      (when (string= result "STORED")
        (return t))  ; Success - exit loop

      ;; Retry on conflict
      (format t "Conflict detected, retrying...~%"))))
```

## Complete Examples

### Example 1: Update User Profile

```lisp
(defun update-user-profile (user-id modifications)
  "Update user profile with CAS for safety."
  (let ((key (format nil "user:~A:profile" user-id)))

    (loop
      (let* ((responses (cl-memcached:mc-gets+ (list key)))
             (response (first responses)))

        (unless response
          (error "User profile not found: ~A" user-id))

        (let* ((cas-token (cl-memcached:mc-cas-unique response))
               (profile-json (babel:octets-to-string
                              (cl-memcached:mc-data response)))
               (profile (json:decode-json-from-string profile-json))
               (updated-profile (apply-modifications profile modifications))
               (updated-json (json:encode-json-to-string updated-profile))
               (result (cl-memcached:mc-cas key updated-json cas-token)))

          (cond
            ((string= result "STORED")
             (format t "Profile updated for user ~A~%" user-id)
             (return updated-profile))

            ((string= result "EXISTS")
             (format t "Conflict for user ~A, retrying...~%" user-id))

            ((string= result "NOT_FOUND")
             (error "Profile deleted during update"))))))))

;; Usage
(update-user-profile "user123"
                     '((:email . "new@example.com")
                       (:phone . "+1-555-1234")))
```

### Example 2: Increment Complex Counter

When simple counters aren't enough (need context):

```lisp
(defun increment-stat-with-timestamp (stat-key)
  "Increment counter and record timestamp using CAS."
  (loop
    (let* ((responses (cl-memcached:mc-gets+ (list stat-key)))
           (response (first responses)))

      (if response
          ;; Key exists - update with CAS
          (let* ((cas-token (cl-memcached:mc-cas-unique response))
                 (stat-data (json:decode-json-from-string
                             (babel:octets-to-string
                              (cl-memcached:mc-data response))))
                 (count (cdr (assoc :count stat-data)))
                 (new-stat (list (cons :count (1+ count))
                                (cons :last-updated (get-universal-time))))
                 (new-json (json:encode-json-to-string new-stat))
                 (result (cl-memcached:mc-cas stat-key new-json cas-token)))

            (when (string= result "STORED")
              (return new-stat))

            ;; Retry on conflict
            (format t "Conflict, retrying...~%"))

          ;; Key doesn't exist - initialize
          (let ((initial-stat (list (cons :count 1)
                                   (cons :last-updated (get-universal-time))))
                (initial-json (json:encode-json-to-string initial-stat)))
            (handler-case
              (progn
                (cl-memcached:mc-add stat-key initial-json)
                (return initial-stat))
              (error ()
                ;; Another thread created it - retry
                nil)))))))

;; Usage
(increment-stat-with-timestamp "api:calls:total")
;; => ((:COUNT . 1) (:LAST-UPDATED . 3912345678))
```

### Example 3: Distributed List Append

Append to a list stored in memcached:

```lisp
(defun append-to-list (list-key new-item)
  "Safely append item to list using CAS."
  (loop
    (let* ((responses (cl-memcached:mc-gets+ (list list-key)))
           (response (first responses)))

      (if response
          ;; List exists - append with CAS
          (let* ((cas-token (cl-memcached:mc-cas-unique response))
                 (current-list (json:decode-json-from-string
                                (babel:octets-to-string
                                 (cl-memcached:mc-data response))))
                 (new-list (append current-list (list new-item)))
                 (new-json (json:encode-json-to-string new-list))
                 (result (cl-memcached:mc-cas list-key new-json cas-token)))

            (when (string= result "STORED")
              (return new-list)))

          ;; List doesn't exist - create
          (let ((initial-list (list new-item))
                (initial-json (json:encode-json-to-string initial-list)))
            (handler-case
              (progn
                (cl-memcached:mc-add list-key initial-json)
                (return initial-list))
              (error ()
                nil)))))))

;; Usage
(append-to-list "recent-events" "user-login")
;; Safe even if multiple processes append simultaneously
```

### Example 4: Meta Protocol CAS

Using modern meta protocol:

```lisp
(defun update-with-meta-cas (key update-fn)
  "Update key using meta protocol CAS."
  (loop
    (multiple-value-bind (response foundp)
        (cl-memcached:mc-meta-get key :cas t)

      (unless foundp
        (error "Key not found: ~A" key))

      (let* ((cas-token (gethash :cas response))
             (current-value (babel:octets-to-string
                            (gethash :value response)))
             (new-value (funcall update-fn current-value))
             (result (cl-memcached:mc-meta-set key new-value :cas cas-token)))

        (cond
          ((string= result "HD")  ; Success in meta protocol
           (return new-value))

          ((string= result "EX")  ; Exists/conflict
           (format t "Conflict, retrying...~%"))

          (t
           (error "Unexpected result: ~A" result)))))))

;; Usage
(update-with-meta-cas "config:version"
                      (lambda (old) (format nil "~A.1" old)))
```

## When to Use CAS

**✓ Use CAS for:**
- Complex data structure updates
- Multi-field modifications
- When simple counters aren't enough
- Implementing distributed algorithms
- Updating JSON/structured data

**✗ Don't use CAS for:**
- Simple counters (use `mc-incr`/`mc-decr` instead)
- Single-writer scenarios (no concurrency)
- Data that doesn't need consistency
- Extremely high-conflict scenarios (use database)

## Performance Characteristics

**Best case (no conflicts):**
- 2 operations: GETS + CAS
- Similar latency to simple SET

**Worst case (high conflicts):**
- Multiple retries
- Degrades with contention
- Consider sharding or database

**Benchmark:**

```lisp
;; Low contention
(time
  (dotimes (i 1000)
    (update-with-cas "low-contention-key" #'modify-fn)))
;; ~500ms with pooling

;; High contention (10 threads competing)
;; Much slower due to retries
```

## Common Mistakes

### Mistake 1: Using mc-get instead of mc-gets

```lisp
;; Wrong - no CAS token!
(let ((response (first (mc-get+ (list "key")))))
  (mc-cas "key" "new" (mc-cas-unique response)))
;; => Error: mc-cas-unique is NIL

;; Right - use mc-gets+
(let ((response (first (mc-gets+ (list "key")))))
  (mc-cas "key" "new" (mc-cas-unique response)))
```

### Mistake 2: Not handling EXISTS response

```lisp
;; Wrong - ignores conflicts
(mc-cas "key" "new-value" cas-token)
;; => Might return "EXISTS", but code doesn't check

;; Right - check response and retry
(let ((result (mc-cas "key" "new-value" cas-token)))
  (when (string= result "EXISTS")
    ;; Retry logic here
    ))
```

### Mistake 3: Infinite retry loop without backoff

```lisp
;; Wrong - spins CPU under high contention
(loop
  (when (try-cas-update)
    (return)))

;; Better - add exponential backoff
(loop for i from 1
      when (try-cas-update)
      return it
      do (sleep (* 0.001 (expt 2 (min i 10)))))  ; Exponential backoff
```

### Mistake 4: Modifying mutable data race

```lisp
;; Wrong - destructive modification
(let ((data (read-data-structure)))
  (setf (slot-value data 'field) 'new-value)  ; Modifies original!
  (cas-update data))

;; Right - create new copy
(let ((data (copy-structure (read-data-structure))))
  (setf (slot-value data 'field) 'new-value)
  (cas-update data))
```

## Troubleshooting

**Problem: Getting NIL for cas-unique**

Cause: Using `mc-get+` instead of `mc-gets+`.

Solution:
```lisp
(mc-gets+ (list "key"))  ; Not mc-get+
```

**Problem: Always getting EXISTS response**

Cause: CAS token is stale or key was modified.

Solution: Ensure you're using fresh CAS token from `mc-gets+` immediately before `mc-cas`.

**Problem: Infinite retry loop**

Cause: Very high contention or broken update logic.

Solution: Add retry limit and exponential backoff:
```lisp
(loop repeat 10  ; Max 10 retries
      for backoff = 0.001 then (* backoff 2)
      when (try-update)
      return it
      do (sleep backoff))
```

**Problem: Getting NOT_FOUND during CAS**

Cause: Key was deleted between GETS and CAS.

Solution: Handle NOT_FOUND response appropriately (recreate or error).

## Advanced: CAS with TTL Refresh

Update data and refresh expiration:

```lisp
(defun update-with-ttl-refresh (key update-fn ttl)
  "Update value and refresh TTL using CAS."
  (loop
    (let* ((response (first (mc-gets+ (list key))))
           (cas-token (when response (mc-cas-unique response))))

      (unless response
        (error "Key not found"))

      (let* ((old-value (babel:octets-to-string (mc-data response)))
             (new-value (funcall update-fn old-value))
             (result (mc-cas key new-value cas-token :timeout ttl)))

        (when (string= result "STORED")
          (return new-value))))))
```

## Verification

Test CAS operations:

```lisp
(defun test-cas ()
  "Verify CAS operations work correctly."
  ;; Setup
  (mc-set "cas-test" "initial")

  ;; Test successful CAS
  (let* ((r1 (first (mc-gets+ (list "cas-test"))))
         (cas1 (mc-cas-unique r1)))
    (assert (string= "STORED" (mc-cas "cas-test" "updated" cas1))))

  ;; Test failed CAS (stale token)
  (let* ((r2 (first (mc-gets+ (list "cas-test"))))
         (cas2 (mc-cas-unique r2)))
    (mc-set "cas-test" "modified-by-other")  ; Invalidate CAS token
    (assert (string= "EXISTS" (mc-cas "cas-test" "will-fail" cas2))))

  ;; Cleanup
  (mc-del "cas-test")

  (format t "CAS tests passed!~%"))

(test-cas)
```

## Recap

You've learned:

✓ Why CAS prevents lost updates in concurrent environments
✓ How to read with `mc-gets+` (with CAS token)
✓ How to write with `mc-cas` (conditional update)
✓ Retry loop pattern for handling conflicts
✓ When to use CAS vs simple counters
✓ Common mistakes and how to avoid them

## What's Next

- [Counters](counters.md) - Simpler atomic operations
- [Error Handling](error-handling.md) - Robust error handling
- [API Reference](../reference/api-reference.md) - Complete CAS API

---

**See also:** [CAS Scenarios](../../canon/features/storage/scenarios/cas-operation.md) (Canon specification)
