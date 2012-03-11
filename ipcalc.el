;; ipcalc.el - IP subnet calculator

;; Filename: elisp-format.el
;; Description: IP calculator
;; Author: "Aleksandar Simic" <asimic@gmail.com>
;; Maintainer: Aleksandar Simic
;; License: BSD
;; Created: 2012-03-11 17:10
;; Version: 0.1.0
;; URL: http://github.org/dotemacs/ipcalc.el
;; Keywords: networking

;;; This file is NOT part of GNU Emacs

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

;;; Commentary:

;; Usage: evaluate (ipcalc "192.168.0.23/21")



(defconst cidr-default 32 "CIDR value")

;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-bin-string (n &optional length)
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
  "Takes a string and inverts 0 to 1 and vice versa"
  (let ((start 0)
        (inverted "")
        (binary num))
    (while (< start (length binary))
      (setq pos (substring binary start (1+ start)))
      (cond ((string= "0" pos)
             (setq inverted
                   (concat inverted (replace-regexp-in-string "0" "1" pos))))
            ((string= "1" (substring binary start (1+ start)))
             (setq inverted
                   (concat inverted (replace-regexp-in-string "1" "0" pos)))))
      (message "in while, start is %d pos is %s" start pos)
    (setq start (1+ start)))
  inverted))

(defun network (ip cidr)
  "Takes IP & CIDR and produces network"
  (let ((cidr-as-int (string-to-int cidr)))
    (concat
     (substring (octets-as-binary
                 (ip-to-octets ip)) 0 cidr-as-int)
     (make-string (- cidr-default cidr-as-int) ?0))))

(defun host+1 (binary-ip)
  "increment the given BINARY-IP by 1"
  (aset binary-ip (- cidr-default 1) ?1)
  binary-ip)

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

(defun ipcalc (ip/cidr)
  "IP calculator"
  (let* ((ip (car (split-string ip/cidr "/")))
         (cidr (car (cdr (split-string ip/cidr "/"))))
         (cidr-int (string-to-int cidr))
         (ip-as-binary (octets-as-binary (ip-to-octets ip)))
         (wildcard (invert-binary
                    (ones-and-pad cidr-int)))
         (buffer "*ipcalc*")
         (net (network ip cidr)))
    (if (get-buffer buffer)
        (kill-buffer buffer))
    (pop-to-buffer buffer)
    (insert
     (format "Address:%15s%37s\n" ip ip-as-binary)
     (format "Netmask:%5s%47s\n" cidr (ones-and-pad cidr-int))
     (format "Wildcard:%51s\n" wildcard)
     (format "=>\nNetwork:%52s\n" net)
     (format "HostMin:%52s\n" (host+1 net))
     (format "HostMax:%52s\n" net cidr)
     (format "Broadcast:%50s\n" (host+1 (host-max net cidr)))
     (format "Hosts/Net: %d\n" (hosts/net cidr-int)))))

;;(ipcalc "192.168.0.23/21")
