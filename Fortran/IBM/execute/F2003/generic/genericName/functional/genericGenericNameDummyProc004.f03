!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             and using dummy proc to distinguish between different proc
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
         procedure, pass :: setprocscalar
         procedure, pass :: setprocarray
         generic :: set => setprocscalar, setprocarray
   end type

   abstract interface
      type(base) function scalarproc ()
         import base
      end function
   end interface

   abstract interface
      type(base) function arrayproc ()
         import base
         allocatable :: arrayproc(:)
      end function
   end interface

   contains

      subroutine setprocscalar( a, b )
         class(base), intent(inout) :: a
         procedure(scalarproc) :: b

         select type ( a )
            type is ( base )
               a = b()
         end select

         print *, 'setprocscalar'

      end subroutine

      subroutine setprocarray( a, b )
         class(base), intent(inout) :: a
         procedure(arrayproc) :: b

         select type ( a )
            type is ( base )
               associate ( g => b() )
                  do i = 1, size(g)
                     a%i = a%i + g(i)%i
                  end do
               end associate
         end select

         print *, 'setprocarray'

      end subroutine

end module

program genericGenericNameDummyProc004
   use m

   interface
      type(base) function scalarbase ()
         import base
      end function
   end interface

   interface
      type(base) function arraybase()
         import base
         allocatable :: arraybase(:)
      end function
   end interface

   interface
      type(base) function scalarbase1 ()
         import base
      end function
   end interface

   interface
      type(base) function arraybase1 ()
         import base
         allocatable :: arraybase1(:)
      end function
   end interface

   type(base), pointer :: b1
   type(base), allocatable :: b2
   type(base) :: b3

   allocate ( b1, b2 )

   call b1%set(scalarbase)
   print *, b1%i

   call b1%set(scalarbase1)
   print *, b1%i

   call b1%set(arraybase)
   print *, b1%i

   call b1%set(arraybase1)
   print *, b1%i

   call b2%set(scalarbase)
   print *, b2%i

   call b2%set(scalarbase1)
   print *, b2%i

   call b2%set(arraybase)
   print *, b2%i

   call b2%set(arraybase1)
   print *, b2%i

   call b3%set(scalarbase)
   print *, b3%i

   call b3%set(scalarbase1)
   print *, b3%i

   call b3%set(arraybase)
   print *, b3%i

   call b3%set(arraybase1)
   print *, b3%i

end program

type(base) function scalarbase ()
   use m, only: base

   scalarbase = base(100)
   print *, 'scalarbase'

end function


type(base) function arraybase()
   use m, only: base
   allocatable :: arraybase(:)

   allocate ( arraybase(5), source = (/base(1),base(2),base(3),base(4),base(5)/) )
   print *, 'arraybase'

end function

type(base) function scalarbase1 ()
   use m, only: base

   scalarbase1=base(200)
   print *, 'scalarbase1'

end function

type(base) function arraybase1 ()
   use m, only: base
   allocatable :: arraybase1(:)

   allocate ( arraybase1(10), source = (/base(1),base(2),base(3),base(4),base(5), base(6),base(7),base(8),base(9),base(10)/) )
   print *, 'arraybase1'

end function
