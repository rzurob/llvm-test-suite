       character abc(4)
       integer iii
       open(11, form="unformatted", asynchronous="yes", access="sequential")
       write(11, id=iii) "1234"
       rewind(11)
       read(11, id=iii) abc
       close(11, status="delete")
       end
