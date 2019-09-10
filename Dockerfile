FROM fiddlerwoaroof/sbcl-static:latest

COPY todo-backend.asd /root/quicklisp/local-projects/todo-backend.asd

RUN mkdir -p /build
COPY build/deps.lisp /build/deps.lisp
RUN /usr/local/bin/sbcl --load /build/deps.lisp --quit

RUN rm /root/quicklisp/local-projects/todo-backend.asd

RUN mkdir -p /root/quicklisp/local-projects/cl-todo-backend/
COPY . /root/quicklisp/local-projects/cl-todo-backend/
WORKDIR /root
COPY build/build.lisp /build/build.lisp
RUN /usr/local/bin/sbcl --disable-debugger --load /build/build.lisp /root/todo-backend

EXPOSE 5000
ENTRYPOINT ["/root/todo-backend"]
