;; ipcalc.el --- IP subnet calculator

;; Filename: ipcalc.el
;; Description: IP calculator
;; Author: "Aleksandar Simic" <asimic@gmail.com>
;; License: BSD
;; Created: 2012-03-11 17:10
;; Version: 0.2.1
;; URL: http://github.org/dotemacs/ipcalc.el
;; Keywords: networking

;;; Commentary:
;;
;; Usage: evaluate (ipcalc "192.168.0.23/21")

;;; This file is NOT part of GNU Emacs
;;
;; Copyright (c) 2012, Aleksandar Simic
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

;;; Code

(defconst cidr-default 32 "CIDR value")

(defun int-to-bin-string (n &optional length)
  ;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
  "Convert integer N to bit string (LENGTH, default 8)."
  (let* ((i    0)
	 (len  (or length 8))
	 (s (make-string len ?0)))
    (while (< i len)
      (if (not (zerop (logand n (ash 1 i))))
          (aset s (- len (1+ i)) ?1))
      (setq i (1+ i))
      )
    s))

(defun octets-as-binary (list-of-octets)
  "return LST of octets as a single binary string"
  (let ((binary ""))
  (while list-of-octets
    (setq binary
          (concat binary
                  (int-to-bin-string
                   (string-to-int (car list-of-octets)))))
      (setq list-of-octets (cdr list-of-octets)))
  binary))

(defun ip-to-octets (ip)
  "split IP address and return the octets"
  (let ((octets (split-string ip "\\.")))
    (if (= 4 (length octets))
        octets
      (message "not correct IP format"))))

(defun ones-and-pad (num)
  "return 1's equal to NUM and pad the rest up to 32"
  (unless (and (<= num cidr-default) (> num 0))
    (error "wrong value provided"))
  (concat (make-string num ?1)
          (make-string (- cidr-default num) ?0)))

(defun invert-binary (num)
  "Invert 1s to 0s and vice versa

  (credit: the simpler version below re-writen by pjb from #emacs
          on freenode)"
  (map 'string
       (lambda (ch) (aref "10" (position ch "01")))
       (remove-if-not (lambda (ch) (position ch "01")) num)))

(defun network (ip cidr)
  "Takes IP & CIDR and produces network"
  (let ((cidr-as-int (string-to-int cidr)))
    (concat
     (substring (octets-as-binary
                 (ip-to-octets ip)) 0 cidr-as-int)
     (make-string (- cidr-default cidr-as-int) ?0))))

(defun host+1 (binary-ip)
  "increment the given BINARY-IP by 1"
  (let ((tmp binary-ip))
  (aset tmp (- cidr-default 1) ?1)
  tmp))

(defun host-max (ip cidr)
  (let ((count (string-to-int cidr))
        (max (- cidr-default 1)))
    (while (< count max)
      (aset ip count ?1)
      (setq count(incf count)))
    ip))

(defun hosts/net (num)
  "Calculate Hosts/Net for the NUM given"
  (- (expt 2 (- cidr-default num)) 2))

(defmacro list-to-string (lst)
  "convert LST to string"
  `(mapconcat
    '(lambda (val)
       (identity (if (integerp val)
                     (int-to-string val)
                   val)))
    ,lst ""))

(defun insert-values (lst)
  "insert given LST"
  (insert
   (list-to-string lst)))

(defun bin-to-int (bin)
  "Convert binary to integer"
  (int-to-string (read (concat "#b" bin))))

(defun binary-to-ip (binary)
  "convert binary to IP address"
  (let* (full-ip
        (count 0)
        (1st-octet (substring binary 0 8))
        (2nd-octet (substring binary 8 16))
        (3rd-octet (substring binary 16 24))
        (4th-octet (substring binary 24 32))
        (octets (mapcar
                 'bin-to-int
                 `(,1st-octet ,2nd-octet ,3rd-octet ,4th-octet))))
    (while (< count 3)
      (setq full-ip (concat full-ip (nth count octets) "."))
      (setq count (incf count)))
    (concat full-ip (car (last octets)))))

(defun ipcalc (ip/cidr)
  "IP calculator"
  (let* ((ip (car (split-string ip/cidr "/")))
         (ip-in-binary (octets-as-binary (ip-to-octets ip)))
         (cidr (car (cdr (split-string ip/cidr "/"))))
         (cidr-int (string-to-int cidr))
         (cidr-binary (ones-and-pad cidr-int))
         (wildcard-binary (invert-binary (ones-and-pad cidr-int)))
         (wildcard-ip (binary-to-ip wildcard-binary))
         (netmask (binary-to-ip (invert-binary wildcard-binary)))
         (net-binary (network ip cidr))
         (net-ip (binary-to-ip net-binary))
         (host-max-binary (host-max (network ip cidr) cidr))
         (host-max-ip (binary-to-ip host-max-binary))
         (host-min-binary (host+1 (network ip cidr)))
         (host-min-ip (binary-to-ip host-min-binary))
         (broadcast-binary (host+1 (host-max net-binary cidr)))
         (broadcast-ip (binary-to-ip broadcast-binary))
         (buffer "*ipcalc*"))
    (if (get-buffer buffer)
        (kill-buffer buffer))
    (pop-to-buffer buffer)
    (insert
     (format "Address:%15s%41s\n" ip ip-in-binary)
     (format "Netmask:%16s = %2s %34s\n" netmask cidr cidr-binary)
     (format "Wildcard:%11s%44s\n" wildcard-ip wildcard-binary)
     (format "=>\nNetwork:%14s%42s\n" net-ip (network ip cidr))
     (format "HostMin:%14s%42s\n" host-min-ip host-min-binary)
     (format "HostMax:%16s%40s\n" host-max-ip host-max-binary)
     (format "Broadcast:%14s%40s\n" broadcast-ip broadcast-binary)
     (format "Hosts/Net: %d\n" (hosts/net cidr-int)))))

(provide 'ipcalc)

;;; ipcalc.el ends here
