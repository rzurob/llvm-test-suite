       integer iii
       character(4) ccc
       logical ll

       open(11, form="unformatted", asynchronous="yes", access="sequential")
       write(11, id=iii, asynchronous="yes" ) "abcd"
       write(11, id=iii, asynchronous="yes" ) "efgh"
       write(11, id=iii, asynchronous="yes" ) "ijkl"

       backspace(11)
       read(11, id=iii, asynchronous="yes") ccc

       wait(11)

       if (ccc .ne. "ijkl") then
         error stop 1
       end if

       write(11, id=iii, asynchronous="yes" ) "mnop"
       inquire(11, asynchronous=ccc, id=iii, pending=ll)

       if (ccc .ne. "YES") then
         error stop 2
       end if

       if (ll .neqv. .false.) then
         error stop 3
       end if

       close(11, status="delete")
       end
