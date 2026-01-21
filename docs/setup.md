# Setup Guide

Complete installation and setup instructions for cl-memcached.

## Prerequisites

### 1. Common Lisp Implementation

Install one of these:

**SBCL (Recommended)**
```bash
# macOS
brew install sbcl

# Ubuntu/Debian
sudo apt-get install sbcl

# Fedora/RHEL
sudo dnf install sbcl
```

**CCL (Clozure Common Lisp)**
```bash
# macOS
brew install clozure-cl

# Linux - download from clozure.com
```

**CMUCL**
```bash
# Download from cmucl.org
```

### 2. Quicklisp

Install Quicklisp (Lisp package manager):

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

Then in SBCL:
```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

### 3. Memcached Server

Install memcached:

**macOS:**
```bash
brew install memcached
brew services start memcached
```

**Ubuntu/Debian:**
```bash
sudo apt-get install memcached
sudo systemctl start memcached
sudo systemctl enable memcached
```

**Fedora/RHEL:**
```bash
sudo dnf install memcached
sudo systemctl start memcached
sudo systemctl enable memcached
```

**Verify memcached is running:**
```bash
echo "stats" | nc localhost 11211
```

You should see statistics output.

## Installation

### Via Quicklisp (Recommended)

```lisp
(ql:quickload :cl-memcached)
```

This automatically installs all dependencies:
- usocket
- babel
- split-sequence
- pooler

### Manual Installation

If not using Quicklisp:

1. Clone the repository:
   ```bash
   git clone https://github.com/quasi/cl-memcached.git
   cd cl-memcached
   ```

2. Install dependencies manually (not recommended)

3. Load the system:
   ```lisp
   (asdf:load-system :cl-memcached)
   ```

## Basic Configuration

### 1. Create Connection

```lisp
;; Load the library
(ql:quickload :cl-memcached)

;; Create global connection
(defvar *cache*
  (cl-memcached:make-memcache :host "localhost" :port 11211))

;; Or set default
(setf cl-memcached:*memcache*
      (cl-memcached:make-memcache :host "localhost" :port 11211))
```

### 2. Enable Connection Pooling

**Recommended for production:**

```lisp
(setf cl-memcached:*mc-use-pool* t)
```

This provides 5-7x performance improvement.

### 3. Configure Pool Size (Optional)

```lisp
(defvar *cache*
  (cl-memcached:make-memcache
    :host "localhost"
    :port 11211
    :pool-size 20))  ; Default is 5
```

### 4. Set Encoding (Optional)

Default is UTF-8:

```lisp
cl-memcached:*mc-default-encoding*  ; => :utf-8

;; Change if needed
(setf cl-memcached:*mc-default-encoding* :latin1)
```

## Verify Installation

### Test Connection

```lisp
(defun test-memcached ()
  "Verify memcached connection works."
  (handler-case
    (progn
      ;; Write test value
      (cl-memcached:mc-set "test-key" "test-value")

      ;; Read it back
      (let ((value (cl-memcached:mc-get-value "test-key")))
        (assert (string= value "test-value") ()
                "Value mismatch: expected 'test-value', got '~A'" value))

      ;; Delete test key
      (cl-memcached:mc-del "test-key")

      (format t "✓ Memcached connection test PASSED~%")
      t)

    (error (e)
      (format *error-output* "✗ Memcached connection test FAILED: ~A~%" e)
      nil)))

;; Run test
(test-memcached)
```

Expected output:
```
✓ Memcached connection test PASSED
T
```

### Run Test Suite

```lisp
(asdf:test-system :cl-memcached)
```

All tests should pass.

## Production Setup

### 1. Configuration File

Create `config.lisp`:

```lisp
(in-package :my-app)

(defparameter *cache-host* "localhost")
(defparameter *cache-port* 11211)
(defparameter *cache-pool-size* 20)

(defun setup-cache ()
  "Initialize memcached connection for production."
  (setf cl-memcached:*mc-use-pool* t)
  (setf cl-memcached:*memcache*
        (cl-memcached:make-memcache
          :host *cache-host*
          :port *cache-port*
          :pool-size *cache-pool-size*))
  (format t "Cache connected to ~A:~A (pool size: ~A)~%"
          *cache-host* *cache-port* *cache-pool-size*))
```

### 2. Environment-Specific Configuration

```lisp
(defun setup-cache ()
  "Setup cache based on environment."
  (let ((env (or (uiop:getenv "APP_ENV") "development")))
    (cond
      ((string= env "production")
       (setf *cache-host* "cache-prod.example.com"
             *cache-pool-size* 50))
      ((string= env "staging")
       (setf *cache-host* "cache-staging.example.com"
             *cache-pool-size* 20))
      (t  ; development
       (setf *cache-host* "localhost"
             *cache-pool-size* 5))))

  (setf cl-memcached:*mc-use-pool* t)
  (setf cl-memcached:*memcache*
        (cl-memcached:make-memcache
          :host *cache-host*
          :port *cache-port*
          :pool-size *cache-pool-size*)))
```

### 3. Initialization on Startup

```lisp
(defun app-startup ()
  "Application startup routine."
  ;; Setup logging
  (setup-logging)

  ;; Connect to database
  (setup-database)

  ;; Connect to cache
  (setup-cache)

  ;; Start web server
  (start-web-server))
```

### 4. Health Checks

```lisp
(defun cache-health-check ()
  "Check if cache is healthy."
  (handler-case
    (progn
      (cl-memcached:mc-set "health-check" "ok" :timeout 10)
      (let ((value (cl-memcached:mc-get-value "health-check")))
        (string= value "ok")))
    (error ()
      nil)))

;; Use in monitoring endpoint
(defun /health ()
  (if (cache-health-check)
      '((:status . "healthy"))
      '((:status . "degraded") (:cache . "unavailable"))))
```

## Memcached Server Configuration

### Basic Memcached Options

Start memcached with:

```bash
memcached -m 64      # Memory limit: 64MB
          -p 11211   # Port: 11211
          -l 0.0.0.0 # Listen on all interfaces
          -c 1024    # Max connections: 1024
          -t 4       # Threads: 4
```

### Production Memcached Systemd Service

Create `/etc/systemd/system/memcached.service`:

```ini
[Unit]
Description=Memcached
After=network.target

[Service]
Type=simple
ExecStart=/usr/bin/memcached -m 512 -p 11211 -u memcached -l 0.0.0.0 -c 2048 -t 8
Restart=always

[Install]
WantedBy=multi-user.target
```

Reload and start:
```bash
sudo systemctl daemon-reload
sudo systemctl enable memcached
sudo systemctl start memcached
```

### Multiple Memcached Instances

For sharding or redundancy:

```lisp
(defvar *cache-shard-1*
  (make-memcache :host "cache1.example.com" :port 11211))

(defvar *cache-shard-2*
  (make-memcache :host "cache2.example.com" :port 11211))

(defun get-cache-for-key (key)
  "Simple key-based sharding."
  (if (evenp (sxhash key))
      *cache-shard-1*
      *cache-shard-2*))

(defun sharded-get (key)
  (mc-get-value key :memcache (get-cache-for-key key)))
```

## Docker Setup

### Docker Compose

`docker-compose.yml`:

```yaml
version: '3'
services:
  memcached:
    image: memcached:latest
    ports:
      - "11211:11211"
    command: memcached -m 64 -c 1024
    restart: unless-stopped

  app:
    build: .
    depends_on:
      - memcached
    environment:
      - MEMCACHED_HOST=memcached
      - MEMCACHED_PORT=11211
```

### Dockerfile for Lisp App

```dockerfile
FROM clfoundation/sbcl:latest

WORKDIR /app
COPY . /app

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --quit

# Load dependencies
RUN sbcl --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :cl-memcached)' \
         --quit

CMD ["sbcl", "--load", "app.lisp"]
```

## Monitoring Setup

### Log Cache Operations

```lisp
(defun logged-mc-set (key value &rest args)
  "Log cache SET operations."
  (format t "[CACHE] SET ~A~%" key)
  (apply #'cl-memcached:mc-set key value args))

(defun logged-mc-get (key &rest args)
  "Log cache GET operations."
  (let ((result (apply #'cl-memcached:mc-get-value key args)))
    (format t "[CACHE] GET ~A => ~A~%" key (if result "HIT" "MISS"))
    result))
```

### Metrics Collection

```lisp
(defvar *cache-metrics* (make-hash-table :test 'equal))

(defun record-cache-hit ()
  (incf (gethash :hits *cache-metrics* 0)))

(defun record-cache-miss ()
  (incf (gethash :misses *cache-metrics* 0)))

(defun get-cache-metrics ()
  (list :hits (gethash :hits *cache-metrics* 0)
        :misses (gethash :misses *cache-metrics* 0)))
```

## Troubleshooting Setup

### Can't Load cl-memcached

**Error:** `System "cl-memcached" not found`

**Solutions:**
1. Install via Quicklisp: `(ql:quickload :cl-memcached)`
2. Update Quicklisp: `(ql:update-all-dists)`
3. Check Quicklisp is installed: `~/quicklisp/`

### Can't Connect to Memcached

**Error:** `MEMCACHED-SERVER-UNREACHABLE`

**Solutions:**
1. Verify memcached running: `echo "stats" | nc localhost 11211`
2. Check host/port correct
3. Check firewall rules

### Dependency Errors

**Error:** `Component "usocket" not found`

**Solution:**
```lisp
(ql:quickload :usocket)
(ql:quickload :babel)
(ql:quickload :split-sequence)
(ql:quickload :pooler)
```

## Next Steps

After setup:

1. **Read Quickstart:** [quickstart.md](quickstart.md)
2. **Enable Pooling:** `(setf *mc-use-pool* t)`
3. **Run Tests:** `(asdf:test-system :cl-memcached)`
4. **Set Up Monitoring:** [monitoring.md](how-to/monitoring.md)
5. **Read Best Practices:** [performance.md](performance.md)

---

**Setup complete?** Try the [Quickstart Guide](quickstart.md)
