       integer iii
       character(10) ccc
       logical ll

       open(11, form="unformatted", asynchronous="yes", access="sequential")
       write(11, id=iii, asynchronous="yes" ) "abcd"

       inquire(11, asynchronous=ccc, id=iii, pending=ll)

       if (ccc .ne. "YES") then
         error stop 1
       end if

       if (ll .neqv. .false.) then
         error stop 2
       end if

       close(11, status="delete")
       end
