;;; ipcalc.el --- IP subnet calculator

;; Filename: ipcalc.el
;; Description: IP calculator
;; Author: "Aleksandar Simic" <asimic@gmail.com>
;; License: BSD
;; Created: 2012-03-11 17:10
;; Version: 0.2.6
;; URL: http://github.com/dotemacs/ipcalc.el
;; Keywords: networking tools
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs
;;
;; Copyright (c) 2012-2020, Aleksandar Simic
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Usage: evaluate (ipcalc "192.168.0.23/21")

;;; Code:

(require 'cl-lib)

(defconst ipcalc--cidr-default 32 "CIDR value.")
(defconst ipcalc--output-buffer-default "*ipcalc*" "Buffer name for outputting results.")

(defun ipcalc-int-to-bin-string (n &optional length)
  ;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
  "Convert integer N to bit string (LENGTH, default 8)."
  (let* ((i  0)
         (len (or length 8))
         (s (make-string len ?0)))
    (while (< i len)
      (if (not (zerop (logand n (ash 1 i))))
          (aset s (- len (1+ i)) ?1))
      (setq i (1+ i)))
    s))

(defun ipcalc-octets-as-binary (list-of-octets)
  "Return LIST-OF-OCTETS as a single binary string."
  (let ((binary ""))
    (while list-of-octets
      (setq binary
            (concat binary
                    (ipcalc-int-to-bin-string
                     (string-to-number (car list-of-octets)))))
      (setq list-of-octets (cdr list-of-octets)))
    binary))

(defun ipcalc-ip-to-octets (ip)
  "Split IP address and return the octets."
  (let ((octets (split-string ip "\\.")))
    (if (= 4 (length octets))
        octets
      (error "not correct IP format"))))

(defun ipcalc-ones-and-pad (num)
  "Return 1's equal to NUM and pad the rest up to
`ipcalc--cidr-default'."
  (unless (and (<= num ipcalc--cidr-default) (> num 0))
    (error "Wrong value provided"))
  (concat (make-string num ?1)
          (make-string (- ipcalc--cidr-default num) ?0)))

(defun ipcalc-invert-binary (num)
  "Invert NUM's 1s to 0s and vice versa.

  (credit: the simpler version below re-writen by pjb from #emacs
          on freenode)"
  (cl-map 'string
          (lambda (ch) (aref "10" (cl-position ch "01")))
          (cl-remove-if-not (lambda (ch) (cl-position ch "01")) num)))

(defun ipcalc-network (ip cidr)
  "Takes IP & CIDR and produces network."
  (let ((cidr-as-int (string-to-number cidr)))
    (concat
     (substring (ipcalc-octets-as-binary
                 (ipcalc-ip-to-octets ip)) 0 cidr-as-int)
     (make-string (- ipcalc--cidr-default cidr-as-int) ?0))))

(defun ipcalc-host+1 (binary-ip)
  "Increment the given BINARY-IP by 1, if even. Leave unchanged
if odd."
  (let ((tmp binary-ip))
    (aset tmp (- ipcalc--cidr-default 1) ?1)
    tmp))

(defun ipcalc-host-max (ip cidr)
  "Given IP and CIDR, return maximum host as a binary value."
  (let ((count (string-to-number cidr))
        (max (- ipcalc--cidr-default 1)))
    (while (< count max)
      (aset ip count ?1)
      (setq count (cl-incf count)))
    ip))

(defun ipcalc-hosts/net (num)
  "Calculate Hosts/Net for the NUM given."
  (- (expt 2 (- ipcalc--cidr-default num)) 2))

(defun ipcalc-bin-to-int (bin)
  "Convert binary value BIN to integer."
  (int-to-string (read (concat "#b" bin))))

(defmacro ipcalc-insert-or-return-value (value &optional cu-arg)
  "If called with given CU-ARG C-u argument (default: '(4)), insert value in current buffer,
else, return VALUE."
  `(if (equal current-prefix-arg (or ,cu-arg '(4)))
       (insert (format "%s" (let ((current-prefix-arg nil)) ,value)))
     (let ((current-prefix-arg nil)) ,value)))

(defun ipcalc-ip-to-binary (ip)
  "Convert IP address to binary string."
  (mapconcat 'ipcalc-int-to-bin-string
             (mapcar 'string-to-number (ipcalc-ip-to-octets ip)) ""))

(defun ipcalc-binary-to-ip (binary)
  "Convert BINARY to IP address."
  (let* (full-ip
         (count 0)
         (1st-octet (substring binary 0 8))
         (2nd-octet (substring binary 8 16))
         (3rd-octet (substring binary 16 24))
         (4th-octet (substring binary 24 32)))
    (mapconcat 'ipcalc-bin-to-int
               `(,1st-octet ,2nd-octet ,3rd-octet ,4th-octet) ".")))

(defun ipcalc-cidr-to-mask (cidr)
  "Convert a CIDR value to a netmask."
  (interactive "nCIDR: ")
  (ipcalc-insert-or-return-value
   (ipcalc-binary-to-ip (ipcalc-ones-and-pad cidr))))

(defun ipcalc-mask-to-cidr (mask)
  "Convert a MASK to a cidr value."
  (interactive "sMASK: ")
  (ipcalc-insert-or-return-value
   (let* ((split (split-string (ipcalc-ip-to-binary mask) "0")))
     (if (delete "" (cdr split))
         (error "Invalid network mask")
       (length (car split))))))

(defun ipcalc-cidr-to-wildcard (cidr)
  "Convert a CIDR value to a wildcard."
  (interactive "nCIDR: ")
  (if (or (eq 0 cidr) (< 32 cidr)) (error "Invalid CIDR"))
  (ipcalc-insert-or-return-value
   (ipcalc-binary-to-ip
    (concat
     (make-string cidr ?0)
     (make-string (- ipcalc--cidr-default cidr) ?1)))))

(defun ipcalc-wildcard-to-cidr (wildcard)
  "Convert a WILDCARD mask to a cidr."
  (interactive "sWILDCARD: ")
  (ipcalc-insert-or-return-value
   (let* ((split (split-string (ipcalc-ip-to-binary wildcard) "1"))
          (cidr (length (car split))))
     (if (delete "" (cdr split))
         (error "Wrong wildcard format")
       cidr))))

(defun ipcalc-ipcidr-to-network (ip/cidr)
  "Convert an IP/CIDR to the corresponding network."
  (interactive "sIP/CIDR: ")
  (ipcalc-insert-or-return-value
   (let* ((split (split-string ip/cidr "/"))
          (ip (car split))
          (cidr (string-to-number (cadr split))))
     (ipcalc-binary-to-ip
      (concat
       (substring (ipcalc-ip-to-binary ip) 0 cidr)
       (make-string (- ipcalc--cidr-default cidr) ?0))))))

(defun ipcalc-ipcidr-to-host-min (ip/cidr)
  "Given a IP/CIDR, get the first possible ip of the network."
  (interactive "sIP/CIDR: ")
  (ipcalc-insert-or-return-value
   (let* ((split (split-string ip/cidr "/"))
          (ip (car split))
          (cidr (string-to-number (cadr split))))
     (ipcalc-binary-to-ip
      (concat
       (substring (ipcalc-ip-to-binary ip) 0 cidr)
       (cl-case cidr
         (32 "")
         (t (make-string (- ipcalc--cidr-default cidr 1) ?0)))
       (cl-case cidr
         (32 "")
         (31 "0")
         (t "1")))))))

(defun ipcalc-ipcidr-to-host-max (ip/cidr)
  "Given a IP/CIDR, get the last possible ip of the network."
  (interactive "sIP/CIDR: ")
  (ipcalc-insert-or-return-value
   (let* ((split (split-string ip/cidr "/"))
          (ip (car split))
          (cidr (string-to-number (cadr split))))
     (ipcalc-binary-to-ip
      (concat
       (substring (ipcalc-ip-to-binary ip) 0 cidr)
       (cl-case cidr
         (32 "")
         (t (make-string (- ipcalc--cidr-default cidr 1) ?1)))
       (cl-case cidr
         (32 "")
         (31 "1")
         (t "0")))))))

(defun ipcalc-ipcidr-to-broadcast (ip/cidr)
  "Convert an IP/CIDR to the corresponding broadcast direction."
  (interactive "sIP/CIDR: ")
  (ipcalc-insert-or-return-value
   (let* ((split (split-string ip/cidr "/"))
          (ip (car split))
          (cidr (string-to-number (cadr split))))
     (ipcalc-binary-to-ip
      (concat
       (substring (ipcalc-ip-to-binary ip) 0 cidr)
       (make-string (- ipcalc--cidr-default cidr) ?1))))))

(defun ipcalc-cidr-to-hosts/net (cidr)
  "Return the number of hosts in the network from the CIDR."
  (interactive "nCIDR: ")
  (ipcalc-insert-or-return-value
   (if (> cidr 30) 0
     (- (expt 2 (- ipcalc--cidr-default cidr)) 2))))

(defun ipcalc-next-ip (ip &optional ip/cidr)
  "Given an IP calculate the next valid one. If an IP/CIDR is
given, the function will throw an error when generating an IP
outside the network range."
  (interactive "sIP: ")
  (ipcalc-insert-or-return-value
   (let ((next-ip
          (ipcalc-binary-to-ip
           (ipcalc-int-to-bin-string
            (1+
             (string-to-number
              (ipcalc-bin-to-int
               (ipcalc-ip-to-binary ip))))
            32 ))))
     (if (and ip/cidr
              (equal next-ip (ipcalc-ipcidr-to-broadcast ip/cidr)))
         (error "Next IP outside of network")
       next-ip))))

(defun ipcalc-ipcidr-to-next-ip (ip/cidr)
  "Given an IP/CIDR, generate the next IP of the network.

Will throw an error if the generated IP is outside the network."
  (interactive "sIP/CIDR: ")
  (ipcalc-insert-or-return-value
   (let ((cidr (cadr (split-string ip/cidr "/"))))
     (format "%s/%s" (ipcalc-next-ip ip/cidr ip/cidr) cidr))))


;;;###autoload
(defun ipcalc (ip/cidr &optional buffer)
  "IP calculator for given IP/CIDR. Insert the output in the buffer
BUFFER (by default, the buffer `ipcalc--output-buffer-default')."
  (interactive "sIP/CIDR: ")
  (let* ((split-input (thread-first (replace-regexp-in-string "\\\"" "" ip/cidr)
                        (split-string "/")))
         (ip (car split-input))
         (cidr (cadr split-input))
         (ip-in-binary (ipcalc-octets-as-binary (ipcalc-ip-to-octets ip)))
         (cidr-int (string-to-number cidr))
         (cidr-binary (ipcalc-ones-and-pad cidr-int))
         (wildcard-binary (ipcalc-invert-binary (ipcalc-ones-and-pad cidr-int)))
         (wildcard-ip (ipcalc-binary-to-ip wildcard-binary))
         (netmask (ipcalc-binary-to-ip (ipcalc-invert-binary wildcard-binary)))
         (net-binary (ipcalc-network ip cidr))
         (net-ip (ipcalc-binary-to-ip net-binary))
         (host-max-binary (ipcalc-host-max (ipcalc-network ip cidr) cidr))
         (host-max-ip (ipcalc-binary-to-ip host-max-binary))
         (host-min-binary (ipcalc-host+1 (ipcalc-network ip cidr)))
         (host-min-ip (ipcalc-binary-to-ip host-min-binary))
         (broadcast-binary (ipcalc-host+1 (ipcalc-host-max net-binary cidr)))
         (broadcast-ip (ipcalc-binary-to-ip broadcast-binary))
         (buffer (or buffer ipcalc--output-buffer-default)))
    (if (and (string-equal buffer ipcalc--output-buffer-default) (get-buffer buffer))
        (kill-buffer buffer))
    (pop-to-buffer buffer)
    (insert
     (format "Address: %17s%40s\n" ip ip-in-binary)
     (format "Netmask: %17s = %2s %34s\n" netmask cidr cidr-binary)
     (format "Wildcard: %16s%40s\n" wildcard-ip wildcard-binary)
     (format "=>\nNetwork: %17s%40s\n" net-ip (ipcalc-network ip cidr))
     (format "HostMin: %17s%40s\n" host-min-ip host-min-binary)
     (format "HostMax: %17s%40s\n" host-max-ip host-max-binary)
     (format "Broadcast: %15s%40s\n" broadcast-ip broadcast-binary)
     (format "Hosts/Net: %d\n" (ipcalc-hosts/net cidr-int)))))

;;;###autoload
(defun ipcalc-current-buffer (ip/cidr)
  "IP calculator for given IP/CIDR. Inserts the output in the
current buffer."
  (interactive "sIP/CIDR: ")
  (ipcalc ip/cidr (buffer-name)))

(provide 'ipcalc)

;;; ipcalc.el ends here
