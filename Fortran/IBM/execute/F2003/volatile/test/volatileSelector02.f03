!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : selector in selectType, VOLATILE
!*
!*  DESCRIPTION                : functional TC
!*
!*     8.1.4.3
!*
!*  The associating entity has the ASYNCHRONOUS, TARGET, or VOLATILE attribute
!*  if and only if the selector is a variable and has the attribute.
!* ===================================================================

  program volatileSelector02

    interface
        function func(x)
            class (*), pointer :: func
            class (*), intent(in) :: x
            VOLATILE:: func
        end function
    end interface

    integer y

    y = 20

    select type (a => func (y))    ! selector is function return
        type is (integer(8))
            print *, a
        type is (integer(4))
            print *, a * 2
    end select

   end program volatileSelector02

   function func(x)
       class (*), pointer :: func
       class (*), intent(in) :: x
       VOLATILE::func

       allocate (func, source=x)
   end function

