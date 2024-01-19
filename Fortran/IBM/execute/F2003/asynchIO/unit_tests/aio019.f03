       module mod
         integer iii, jjj, hhh
       end module

       use mod
       character(10) :: ccc
       logical ll

       hhh = 11
       open(hhh, form="unformatted", asynchronous="yes", access="sequential")
       write(hhh, id=iii )  "abcd"
       call sub1()
       call sub2()

       inquire(hhh, asynchronous=ccc, pending=ll)

       if (ccc .ne. "YES") then
         error stop 1
       end if

       if (ll .neqv. .false.) then
         error stop 2
       end if

       close(hhh, status="delete")
       end

       subroutine sub1()
       use mod
       write(hhh, id=jjj )  "efgh"
       end subroutine

       subroutine sub2()
       use mod
       wait(11, id=iii)
       wait(11, id=jjj)
       end subroutine
