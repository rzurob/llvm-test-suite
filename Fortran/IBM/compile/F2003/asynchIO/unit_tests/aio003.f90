       character abc(10)
       integer iii
       write(11, id=iii, asynchronous="NO") "1234"
       read(11, id=iii, asynchronous="NO") abc
       close(11, status="delete")
       end
