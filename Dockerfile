FROM fiddlerwoaroof/sbcl-static:1.5.6

COPY build /build

RUN /usr/local/bin/sbcl --load /build/build.lisp

ENTRYPOINT ["/usr/local/bin/sbcl"]
