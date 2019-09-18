(ql:quickload :usocket)


(defvar master-socket
      (usocket:socket-listen "127.0.0.1" 1112
                             :reuse-address t
                             :element-type 'unsigned-byte))

(usocket:wait-for-input master-socket)

(defvar new-client (usocket:socket-accept master-socket))

(usocket:socket-accept master-socket)

(format (usocket:socket-stream new-client) "Hello~%")

(format t "hello~%")
