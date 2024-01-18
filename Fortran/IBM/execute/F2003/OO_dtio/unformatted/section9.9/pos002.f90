!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: pos002.f
! %VERIFY: pos002.out:pos002.vf
! %STDIN:
! %STDOUT: pos002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - POS= specifier: Try using the pos specifier inside DTIO and main program
!*                                                 Try it with backspace, rewind, endfile, flush
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type, abstract :: base
      character(3) :: c
   contains
      procedure, pass :: getc
      procedure(intf), deferred :: geti
   end type

   type, extends(base) :: child
      integer(4) :: i
   contains
      procedure, pass :: geti
   end type

   type, extends(child) :: gen3
      ! empty
   end type

   interface
      function intf(a)
         import base
         class(base), intent(in) :: a
         integer(4) :: intf
      end function
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   function getc(a)
      class(base), intent(in) :: a
      character(3) :: getc
      getc = a%c
   end function

   function geti(a)
      class(child), intent(in) :: a
      integer(4) :: geti
      geti = a%i
   end function

end module


program pos002
   use m1

   ! declaration of variables
   class(base), allocatable :: b1, b2
   class(base), pointer     :: b3, b4(:,:)
   character(200) :: msg1 = ''
   integer :: stat1

   character(10) :: access1
   integer(4)    :: pos1
   integer(4)    :: size1

   ! allocation of variables

   allocate ( b1, source = child( 'abc',101 ) )
   allocate ( b2, source = gen3( 'def', 102 ) )
   allocate ( b3, source = gen3( 'ghi', 103 ) )
   allocate ( b4(2,2), source = reshape ( source=(/ b2, b3, b2, b3 /), shape=(/2,2/) ) )

   ! I/O operations

   open ( 1, file = 'pos002.data', form='unformatted', access='stream' )

   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )     !<- check the initial pos and access mode for unit1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 1_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) )                    error stop 2_4

   write ( 1, iostat=stat1, iomsg=msg1 )                 b1                 !<- write to file starting at pos 1
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 3_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 8 ) )                    error stop 4_4

   write ( 1, iostat=stat1, iomsg=msg1, pos=15 )         b2
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 5_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 22 ) )                   error stop 6_4

   write ( 1, iostat=stat1, iomsg=msg1, pos=8 )          b3
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 7_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 15 ) )                   error stop 8_4

   write ( 1, iostat=stat1, iomsg=msg1, pos=22 )         b4 (1:2,1)   !<- writes def, 102, ghi, 103
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 9_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 36 ) .or. ( size1 /= 35 ) )                    error stop 10_4

   endfile 1

   rewind 1

   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 11_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) .or. ( size1 /= 35 ) )                    error stop 12_4  !<- pos of the file should be reset to 1

   FLUSH 1

   read ( 1, iostat = stat1, iomsg=msg1, pos = 1 )                      b4
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 13_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 14_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 29 ) )                   error stop 15_4

   read ( 1, iostat = stat1, iomsg=msg1, pos = 8 )                       b1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 16_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 17_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 15 ) )                   error stop 18_4

   read ( 1, iostat = stat1, iomsg=msg1, pos = 1 )                      b3
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 19_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 20_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 8 ) )                    error stop 21_4


   ! check if the values are set correctly


   print *, b1%getc(), b1%geti()                ! ghi 103
   print *, b3%getc(), b3%geti()                ! abc 101
   print *, b4(1,1)%getc(), b4(1,1)%geti()      ! abc 101
   print *, b4(2,1)%getc(), b4(2,1)%geti()      ! ghi 103
   print *, b4(1,2)%getc(), b4(1,2)%geti()      ! def 102
   print *, b4(2,2)%getc(), b4(2,2)%geti()      ! def 102

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: access1
   integer(4)    :: pos1, pos2

   inquire ( unit, access = access1, pos = pos1 )

   read ( unit, iostat = iostat, iomsg = iomsg )   dtv%c

   if ( iostat /= 0 )         error stop 22_4

   flush ( unit, iostat = iostat )

   if ( iostat /= 0 )         error stop 23_4

   select type ( dtv )
      class is (child)
         read ( unit, iostat = iostat ) dtv%i
         if ( iostat /= 0 )   error stop 24_4
         flush ( unit, iostat = iostat )
         if ( iostat /= 0 )   error stop 25_4
   end select

   inquire ( unit, pos = pos2 )

   if ( pos2 /= pos1 + 7 )    error stop 26_4    !<- non-abstract child type contains 6 characters (therefore, always 6 pos written)

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: access1
   integer(4)    :: pos1, pos2

   inquire ( unit, access = access1, pos = pos1 )

   write   ( unit, iostat = iostat, iomsg = iomsg )   dtv%getc()

   if ( iostat /= 0 )         error stop 27_4

   flush ( unit, iostat = iostat )

   if ( iostat /= 0 )         error stop 28_4

   select type ( dtv )
      class is (child)
         write ( unit, iostat = iostat ) dtv%geti()
         if ( iostat /= 0 )   error stop 29_4
         flush ( unit, iostat = iostat )
         if ( iostat /= 0 )   error stop 30_4
   end select

   inquire ( unit, pos = pos2 )

   if ( pos2 /= pos1 + 7 )    error stop 31_4    !<- non-abstract child type contains 6 characters (therefore, always 6 pos written)

end subroutine