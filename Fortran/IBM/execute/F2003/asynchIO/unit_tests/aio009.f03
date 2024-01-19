       integer iii
       character(10) ccc
       logical :: ll = .false.

       open(11, form="unformatted", asynchronous="yes", access="sequential")
       write(11, id=iii, asynchronous="yes" ) "abcd"

       do while (ll .eqv. .false.)
         wait(11, id=iii, done=ll)
       end do

       if (ll .neqv. .true.) then
         error stop 1
       end if

       write(11, id=iii, asynchronous="yes" ) "efgh"
       inquire(11, asynchronous=ccc, id=iii, pending=ll)

       if (ccc .ne. "YES") then
         error stop 2
       end if

       if (ll .neqv. .false.) then
         error stop 3
       end if

       close(11, status="delete")
       end
