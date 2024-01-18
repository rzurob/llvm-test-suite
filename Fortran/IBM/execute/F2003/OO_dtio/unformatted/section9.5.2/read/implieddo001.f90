!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: implieddo001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an implied-do-list
!*                               Sequential Access
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
   type base
      character(3) :: c = 'xxx'
   end type

   type,extends(base) :: child
      character(3) :: c1 = 'XXX'
   end type

end module


program implieddo001
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable     :: b1(:)
   class(base), allocatable     :: b2(:,:)
   type(base) :: b3(4)
   type(base),  pointer :: b4(:,:)
   integer :: stat
   character(200) :: msg
   character(12)  :: c1
   character(24)  :: c2
   character(6)   :: c3
   character(3)   :: c4

   ! allocation of variables
   allocate ( b1(4) )
   allocate ( b2(2,2) )
   allocate ( b4(2,2) )

   open (unit = 1, file ='implieddo001.1', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )            'abcdefghijkl'
   write (1, iostat=stat, iomsg=msg )            'GHIghiJKLjklDEFdefABCabc'
   write (1, iostat=stat, iomsg=msg )            'vwxpqr'
   write (1, iostat=stat, iomsg=msg )            'VWX'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             ( b1, i= 1, 1, -1 )   !<- iteration count is 1
   if ( ( stat /= 0 )  .or. ( msg /= 'dtioread' ) )  error stop 1_4
   read (1, iostat=stat, iomsg=msg )             ( ( b2(i,j), i = 1, 2), j = 1, 2 )
   if ( ( stat /= 0 )  .or. ( msg /= 'dtioread' ) )  error stop 2_4
   read (1, iostat=stat, iomsg=msg )             ( b3(k), k=4_4,2_2,-2_8 )
   if ( ( stat /= 0 )  .or. ( msg /= 'dtioread' ) )  error stop 3_4
   read (1, iostat=stat, iomsg=msg )             ( b4(2,2), j=1,2,3 )
  if ( ( stat /= 0 )  .or. ( msg /= 'dtioread' ) )  error stop 4_4

   ! check if the values are set correctly

   if ( (b1(1)%c /= 'abc') .or. (b1(2)%c /= 'def') .or. (b1(3)%c /= 'ghi') .or. (b1(4)%c /= 'jkl') )             error stop 5_4
   select type (b2)
      type is (child)
         if ( (b2(1,1)%c /= 'ABC') .or. (b2(1,1)%c1 /= 'abc') .or. (b2(2,1)%c /= 'DEF') .or. (b2(2,1)%c1 /= 'def') .or. &
              (b2(1,2)%c /= 'GHI') .or. (b2(1,2)%c1 /= 'ghi') .or. (b2(2,2)%c /= 'JKL') .or. (b2(2,2)%c1 /= 'jkl') )   error stop 6_4
   end select

   if ( (b3(1)%c /= 'xxx') .or. (b3(2)%c /= 'pqr') .or. (b3(3)%c /= 'xxx') .or. (b3(4)%c /= 'vwx') )             error stop 7_4
   if ( (b4(1,1)%c /= 'xxx') .or. (b4(2,1)%c /= 'xxx') .or. (b4(1,2)%c /= 'xxx') .or. (b4(2,2)%c /= 'VWX')  )    error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat) dtv%c
   if ( iostat /= 0 ) error stop 9_4

   select type ( dtv )
      type is (child)
         read (unit, iostat = iostat) dtv%c1
   end select

   iomsg = 'dtioread'

end subroutine
