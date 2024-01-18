       character abc(10)
       open(11, form="unformatted", asynchronous="yes", access="sequential")
       write(11, asynchronous="wrong") "1234"
       read(11, asynchronous="wrong") abc
       close(11, status="delete")
       end
