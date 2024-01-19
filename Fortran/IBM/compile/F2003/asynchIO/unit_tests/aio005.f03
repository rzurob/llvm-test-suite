       character abc(10)
       integer iii
       open(11, form="unformatted", asynchronous="no", access="sequential")
       write(11, id=iii) "1234"
       read(11, id=iii) abc
       close(11, status="delete")
       end
