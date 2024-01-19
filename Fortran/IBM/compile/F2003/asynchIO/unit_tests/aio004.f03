       character abc(10)
       open(11, form="unformatted", asynchronous="yes", access="sequential")
       write(11, asynchronous="yes", asynchronous="no") "1234"
       read(11, asynchronous="yes", asynchronous="no") abc
       close(11, status="delete")
       end
