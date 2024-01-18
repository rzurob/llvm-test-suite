        ! Testing some basic argument passing functionality when the dummy
        ! argument is a coarray assumed shape array.
        program abc

        implicit none

        integer, save :: myarray(10)[*]

        myarray = -1

        if(this_image() .eq. 5) then
           myarray = -3
        end if

        sync all

        if (this_image() .eq. 1) then
           myarray = -4
           call sub(myarray)
        end if
        
        sync all

        print *, this_image(), ":", myarray

        sync all

        contains
        subroutine sub(w)
         integer, intent(inout) :: w(:)[*]
         integer i,j

         if (any(w .ne. -4)) then
            error stop 1
         end if

         i = -2
         w(3)[4] = i
         w(6)[4] = -2
         w(3) = -5
         w(6) = -5

         j = w(4)[5]
         if (j .ne. -3) then
            error stop 2
         end if

         j = w(8)[5]
         if (j .ne. -3) then
            error stop 3
         end if

        end subroutine

        end

