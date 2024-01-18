!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             and using dummy proc with function result of different types
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer(4) :: i
      contains
         procedure, pass :: setprocbase
         procedure, pass :: setprocint
         procedure, pass :: setprocreal

         generic :: set => setprocbase, setprocint, setprocreal

   end type

   abstract interface
      type(base) function baseproc(i)
         import base
         integer, intent(in) :: i
      end function
   end interface

   abstract interface
      integer function intproc (i)
         integer, intent(in) :: i
      end function
   end interface

   abstract interface
      real function realproc (i)
         real, intent(in) :: i
      end function
   end interface

   contains

      subroutine setprocbase( a, b, c )
         class(base), intent(inout) :: a
         procedure(baseproc) :: b
         integer, intent(in) :: c

         print *, 'setprocbase'
         select type ( a )
            type is ( base )
               a = b(c)
         end select

      end subroutine

      subroutine setprocint( a, b, c )
         class(base), intent(inout) :: a
         procedure(intproc) :: b
         integer, intent(in) :: c

         print *, 'setprocint'
         select type ( a )
            type is ( base )
               a%i = b(c)
         end select

      end subroutine

      subroutine setprocreal( a, b, c )
         class(base), intent(inout) :: a
         procedure(realproc) :: b
         real, intent(in) :: c

         print *, 'setprocreal'
         select type ( a )
            type is ( base )
               a%i = int(b(c),kind=4)
         end select

      end subroutine

      type(base) function returnnegbase(i)
         integer, intent(in) :: i

         print *, 'returnnegbase'
         returnnegbase = base(-1*i)

      end function

      integer function returnnegint(i)
         integer, intent(in) :: i

         print *, 'returnnegint'
         returnnegint = -1*i

      end function

      real function returnnegreal (i)
         real, intent(in) :: i

         print *, 'returnnegreal'
         returnnegreal = -1*i

      end function

end module

program genericGenericNameDummyProc005
   use m

   interface
      type(base) function returnbase(i)
         import base
         integer, intent(in) :: i
      end function
   end interface

   interface
      integer function returnint(i)
         integer, intent(in) :: i
      end function
   end interface

   interface
      real function returnreal (i)
         real, intent(in) :: i
      end function
   end interface

   type(base), allocatable :: b1
   type(base) :: b2

   allocate ( b1 )

   call b1%set(returnbase,10)
   print *, b1%i

   call b1%set(returnnegbase,100)
   print *, b1%i

   call b1%set(returnint, 500)
   print *, b1%i

   call b1%set(returnnegint, 1000)
   print *, b1%i

   call b1%set(returnreal, 200.0_4)
   print *, b1%i

   call b1%set(returnnegreal, 400.0)
   print *, b1%i

   call b2%set(returnbase,100)
   print *, b2%i

   call b2%set(returnnegbase,1000)
   print *, b2%i

   call b2%set(returnint, 5000)
   print *, b2%i

   call b2%set(returnnegint, 10000)
   print *, b2%i

   call b2%set(returnreal, 2000.0_4)
   print *, b1%i

   call b2%set(returnnegreal, 4000.0)
   print *, b2%i

end program

type(base) function returnbase(i)
   use m, only: base
   integer, intent(in) :: i

   print *, 'returnbase'
   returnbase = base(i)

end function

integer function returnint(i)
   integer, intent(in) :: i

   print *, 'returnint'
   returnint = i

end function

real function returnreal (i)
   real, intent(in) :: i

   print *, 'returnreal'
   returnreal = i

end function
