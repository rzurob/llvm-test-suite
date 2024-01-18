! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/genericName/functional/genericGenericNameDummyProc006.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             and using dummy proc with function result of different kinds
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: setbasewint1
         procedure, pass :: setbasewint2
         procedure, pass :: setbasewint4
         procedure, pass :: setbasewint8

         generic :: set => setbasewint1, setbasewint2, setbasewint4, setbasewint8

   end type

   abstract interface
      integer(1) function int1(i)
         integer(4), intent(in) :: i
      end function
   end interface

   abstract interface
      integer(2) function int2 (i)
         integer(4), intent(in) :: i
      end function
   end interface

   abstract interface
      integer(4) function int4(i)
         integer(4), intent(in) :: i
      end function
   end interface

   abstract interface
      integer(8) function int8 (i)
         integer(4), intent(in) :: i
      end function
   end interface

   contains

      subroutine setbasewint1( a, b, c )
         class(base(4)), intent(inout) :: a
         procedure(int1) :: b
         integer, intent(in) :: c

         print *, 'setbasewint1'
         select type ( a )
            type is ( base(4) )
               a = base(4)( int( b(c), kind=4) )
         end select

      end subroutine

      subroutine setbasewint2( a, b, c )
         class(base(4)), intent(inout) :: a
         procedure(int2) :: b
         integer, intent(in) :: c

         print *, 'setbasewint2'
         select type ( a )
            type is ( base(4) )
               a = base(4)( int( b(c), kind=4) )
         end select

      end subroutine

      subroutine setbasewint4( a, b, c )
         class(base(4)), intent(inout) :: a
         procedure(int4) :: b
         integer, intent(in) :: c

         print *, 'setbasewint4'
         select type ( a )
            type is ( base(4) )
               a = base(4)( int( b(c), kind=4) )
         end select

      end subroutine

      subroutine setbasewint8( a, b, c )
         class(base(4)), intent(inout) :: a
         procedure(int8) :: b
         integer, intent(in) :: c

         print *, 'setbasewint8'
         select type ( a )
            type is ( base(4) )
               a = base(4)( int( b(c), kind=4) )
         end select

      end subroutine

end module

program genericGenericNameDummyProc006
   use m

   interface
      integer(1) function returnint1(i)
         integer, intent(in) :: i
      end function
   end interface

   interface
      integer(2) function returnint2(i)
         integer, intent(in) :: i
      end function
   end interface

   interface
      integer(4) function returnint4(i)
         integer, intent(in) :: i
      end function
   end interface

   interface
      integer(8) function returnint8(i)
         integer, intent(in) :: i
      end function
   end interface

   type(base(4)) :: b1
   class(base(4)), allocatable :: b2
   type(base(4)), pointer :: b3

   allocate ( b2, b3 )

   call b1%set( returnint1, 10_4 )
   print *, b1%i

   call b1%set( returnint2, 100_4 )
   print *, b1%i

   call b1%set( returnint4, 1000_4 )
   print *, b1%i

   call b1%set( returnint8, 10000_4 )
   print *, b1%i

   call b2%set( returnint1, 20_4 )
   print *, b2%i

   call b2%set( returnint2, 200_4 )
   print *, b2%i

   call b2%set( returnint4, 2000_4 )
   print *, b2%i

   call b2%set( returnint8, 20000_4 )
   print *, b2%i

   call b3%set( returnint1, 30_4 )
   print *, b3%i

   call b3%set( returnint2, 300_4 )
   print *, b3%i

   call b3%set( returnint4, 3000_4 )
   print *, b3%i

   call b3%set( returnint8, 30000_4 )
   print *, b3%i

   associate ( ggg => b1 )
      call ggg%set( returnint1, 40_4 )
      print *, ggg%i, b1%i

      call ggg%set( returnint2, 400_4 )
      print *, ggg%i, b1%i

      call ggg%set( returnint4, 4000_4 )
      print *, ggg%i, b1%i

      call ggg%set( returnint8, 40000_4 )
      print *, ggg%i, b1%i
   end associate

end program

integer(1) function returnint1(i)
   integer, intent(in) :: i

   returnint1 = INT(i,kind=1)
   print *, 'returnint1'
end function

integer(2) function returnint2(i)
   integer, intent(in) :: i

   returnint2 = INT(i,kind=2)
   print *, 'returnint2'
end function

integer(4) function returnint4(i)
   integer, intent(in) :: i

   returnint4 = INT(i,kind=4)
   print *, 'returnint4'
end function

integer(8) function returnint8(i)
   integer, intent(in) :: i

   returnint8 = INT(i,kind=8)
   print *, 'returnint8'
end function
