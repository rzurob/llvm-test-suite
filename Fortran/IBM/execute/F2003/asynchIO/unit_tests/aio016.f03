       integer iii, jjj
       character(10) ccc, fmt
       logical ll

       open(11, form="formatted", asynchronous="yes", access="sequential")
       write(11,"(""Test:"",I4,1x,A)", id=iii, asynchronous="yes") 0,fmt
       write(11,"(""Test:"",I4,1x,A)", id=jjj) 0,fmt

       wait(11, id=iii)
       wait(11, id=jjj)

       inquire(11, asynchronous=ccc, pending=ll)

       if (ccc .ne. "YES") then
         error stop 1
       end if

       if (ll .neqv. .false.) then
         error stop 2
       end if

       close(11, status="delete")

       end
