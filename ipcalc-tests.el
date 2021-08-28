
;; eval this buffer to run the tests

(load-file "ipcalc.el")
(require 'ipcalc)


(defun ipcalc-random-ip ()
  "Generate a random valid ip."
  (let ((ip (list (random 255) (random 255) (random 255) (random 255))))
    (mapconcat 'number-to-string ip ".")))


(ert-deftest int-to-bin-string-test ()
  "Test the conversion of a integer to a binary"
  (should (equal "00000000" (ipcalc-int-to-bin-string 0)))
  (should (equal "00000001" (ipcalc-int-to-bin-string 1)))
  (should (equal "01010101" (ipcalc-int-to-bin-string 85)))
  (should (equal "10101010" (ipcalc-int-to-bin-string 170)))
  (should (equal "11111111" (ipcalc-int-to-bin-string 255))))

(ert-deftest network-test ()
  "Test the network function"
  (should (string-equal (ipcalc-network "192.168.0.23" "21")
                        "11000000101010000000000000000000"))
  (should (string-equal (ipcalc-network "128.0.0.0" "1")
                        "10000000000000000000000000000000"))
  (should (string-equal (ipcalc-network "0.0.0.0" "12")
                        "00000000000000000000000000000000"))
  (should (string-equal (ipcalc-network "255.255.255.255" "12")
                        "11111111111100000000000000000000"))
  (should (string-equal (ipcalc-network "255.255.255.255" "16")
                        "11111111111111110000000000000000"))
  (should (string-equal (ipcalc-network "255.255.255.255" "32")
                        "11111111111111111111111111111111"))
  (should (string-equal (ipcalc-network "170.170.170.170" "16")
                        "10101010101010100000000000000000"))
  (should (string-equal (ipcalc-network "170.170.170.170" "16")
                        "10101010101010100000000000000000"))
  (should-error (ipcalc-network "123.123.123" "0"))
  (should-error (ipcalc-network "123.123.123.123.123" "512")))


(ert-deftest octets-as-binary-test ()
  (should (string-equal (ipcalc-octets-as-binary '("192" "168" "0" "23"))
                        "11000000101010000000000000010111"))
  (should (string-equal (ipcalc-octets-as-binary '("1" "1" "1" "1"))
                        "00000001000000010000000100000001"))
  (should (string-equal (ipcalc-octets-as-binary '("170" "170" "170" "170"))
                        "10101010101010101010101010101010"))
  (should (string-equal (ipcalc-octets-as-binary '("85" "85" "85" "85"))
                        "01010101010101010101010101010101"))
  (should (string-equal (ipcalc-octets-as-binary '("255" "255" "255" "255"))
                        "11111111111111111111111111111111")))

(ert-deftest ip-to-octets-test ()
  "That a IP address gets split into octets"
  (should (equal (ipcalc-ip-to-octets "192.168.0.23")
                 '("192" "168" "0" "23")))
  (should (equal (ipcalc-ip-to-octets "1.1.1.1")
                 '("1" "1" "1" "1")))
  (should-error (ipcalc-ip-to-octets "1.1.1"))
  (should-error (ipcalc-ip-to-octets "1.1.1.1.1")))

(ert-deftest ones-and-pad-test ()
  "That padding occurs for a given value"
  (should (string-equal "10000000000000000000000000000000" (ipcalc-ones-and-pad 1)))
  (should (string-equal "11111111111111111111111111111110" (ipcalc-ones-and-pad 31)))
  (should (string-equal "11111111111111111111111111111111" (ipcalc-ones-and-pad 32)))
  (should-error (ipcalc-ones-and-pad 33))
  (should-error (ipcalc-ones-and-pad 0)))

(ert-deftest invert-binary-test ()
  "Tests that it inverts 1s & 0s"
  (should (string-equal (ipcalc-invert-binary "1") "0"))
  (should (string-equal (ipcalc-invert-binary "0") "1"))
  (should (string-equal (ipcalc-invert-binary "00000000000000000000000000000000") "11111111111111111111111111111111"))
  (should (string-equal (ipcalc-invert-binary "11111111111111111111111111111111") "00000000000000000000000000000000"))
  (should (string-equal (ipcalc-invert-binary "00000000000000001111111111111111") "11111111111111110000000000000000"))
  (should (string-equal (ipcalc-invert-binary "11111111111111110000000000000000") "00000000000000001111111111111111"))
  (should (string-equal (ipcalc-invert-binary "10101010101010101010101010101010") "01010101010101010101010101010101"))
  (should (string-equal (ipcalc-invert-binary "01010101010101010101010101010101") "10101010101010101010101010101010")))


(ert-deftest host+1-test ()
  "Add 1 to an binary number"
  (should (equal (ipcalc-host+1 "11000000101010000000000000000000")
                 "11000000101010000000000000000001"))
  (should (equal (ipcalc-host+1 "00000000000000000000000000000000")
                 "00000000000000000000000000000001"))
  (should (equal (ipcalc-host+1 "00000000000000000000000000000001")
                 "00000000000000000000000000000001"))
  (should (equal (ipcalc-host+1 "01111111111111111111111111111110")
                 "01111111111111111111111111111111")))

(ert-deftest host-max-test ()
  "Return the maximum host as a binary value"
  (should (equal (ipcalc-host-max "11000000101010000000000000000000" "21")
                 "11000000101010000000011111111110"))
  (should (equal (ipcalc-host-max "00000000000000000000000000000000" "16")
                 "00000000000000001111111111111110"))
  (should (equal (ipcalc-host-max "11111111111111111111111111111111" "16")
                 "11111111111111111111111111111111")))

(ert-deftest ip-to-binary-test ()
  "Convert an IP to a binary string"
  (should (equal (ipcalc-ip-to-binary "192.168.0.23")
                 "11000000101010000000000000010111"))
  (should (equal (ipcalc-ip-to-binary "1.1.1.1")
                 "00000001000000010000000100000001"))
  (should (equal (ipcalc-ip-to-binary "170.170.170.170")
                 "10101010101010101010101010101010"))
  (should (equal (ipcalc-ip-to-binary "0.0.0.0")
                 "00000000000000000000000000000000"))
  (should (equal (ipcalc-ip-to-binary "255.255.255.255")
                 "11111111111111111111111111111111"))
  (should-error (ipcalc-ip-binary-to "1.1.1")))

(ert-deftest binary-to-ip-test ()
  "Convert an IP in binary format to a IP string"
  (should (equal (ipcalc-binary-to-ip "11000000101010000000000000010111")
                 "192.168.0.23"))
  (should (equal (ipcalc-binary-to-ip "00000001000000010000000100000001")
                 "1.1.1.1"))
  (should (equal (ipcalc-binary-to-ip "10101010101010101010101010101010")
                 "170.170.170.170"))
  (should (equal (ipcalc-binary-to-ip "00000000000000000000000000000000")
                 "0.0.0.0"))
  ;; I think this should be an error
  (should (equal (ipcalc-binary-to-ip "1111111111111111111111111111111100000000000000000000000000000000")
                 "255.255.255.255"))
  (should-error (ipcalc-binary-to-ip "0000000000000000")))

(ert-deftest ip-to-binary-is-reversible-test ()
  "Test that binary-to-ip is the reverse of ip-to-binary"
  (dotimes (number 10)
    (let ((ip (ipcalc-random-ip)))
      (should (equal (ipcalc-binary-to-ip (ipcalc-ip-to-binary ip))
                     ip)))))

(ert-deftest ipcalc-test ()
  "Check that the output should be correctly formatted"
  (let ((temp-buffer-name (make-temp-name "ipcalc-tests-")))
    (with-current-buffer (get-buffer-create temp-buffer-name)
      (ipcalc "192.168.1.1/24" (buffer-name))
      (should (equal (buffer-string) "Address:       192.168.1.1        11000000101010000000000100000001
Netmask:     255.255.255.0 = 24   11111111111111111111111100000000
Wildcard:        0.0.0.255        00000000000000000000000011111111
=>
Network:       192.168.1.0        11000000101010000000000100000000
HostMin:       192.168.1.1        11000000101010000000000100000001
HostMax:     192.168.1.254        11000000101010000000000111111110
Broadcast:   192.168.1.255        11000000101010000000000111111111
Hosts/Net: 254
"))
      (kill-buffer temp-buffer-name))))

(ert-deftest ipcalc-test-*ipcalc*-buffer ()
  "Check that the output should be correctly formatted"
  (ipcalc "192.168.1.1/24")
  (with-current-buffer (get-buffer-create "*ipcalc*")
    (should (equal (buffer-string) "Address:       192.168.1.1        11000000101010000000000100000001
Netmask:     255.255.255.0 = 24   11111111111111111111111100000000
Wildcard:        0.0.0.255        00000000000000000000000011111111
=>
Network:       192.168.1.0        11000000101010000000000100000000
HostMin:       192.168.1.1        11000000101010000000000100000001
HostMax:     192.168.1.254        11000000101010000000000111111110
Broadcast:   192.168.1.255        11000000101010000000000111111111
Hosts/Net: 254
"))))

(ert-deftest ipcalc-current-buffer-test ()
  "Check that the output should be correctly formatted"
  (let ((temp-buffer-name (make-temp-name "ipcalc-tests-")))
    (with-current-buffer (get-buffer-create temp-buffer-name)
      (ipcalc-current-buffer "192.168.1.1/24")
      (should (equal (buffer-string) "Address:       192.168.1.1        11000000101010000000000100000001
Netmask:     255.255.255.0 = 24   11111111111111111111111100000000
Wildcard:        0.0.0.255        00000000000000000000000011111111
=>
Network:       192.168.1.0        11000000101010000000000100000000
HostMin:       192.168.1.1        11000000101010000000000100000001
HostMax:     192.168.1.254        11000000101010000000000111111110
Broadcast:   192.168.1.255        11000000101010000000000111111111
Hosts/Net: 254
"))
      (kill-buffer temp-buffer-name))))

(ert t)
