! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be polymorphic arrays with array section
!*                                 - vector subscripts, elements (with class hierarchy)
!*                               Direct Access
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
      character(3) :: c = ''
      contains
         procedure, pass :: getC
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

end module


program array001a
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base) , allocatable :: b1(:)
   class(child) , allocatable :: b2(:,:)
   class(base) , pointer     :: b3(:)
   class(child), pointer     :: b4(:,:)
   integer :: stat
   character(200) :: msg
   character(28)  :: c1
   character(14)  :: c2
   character(7)  :: c3
   character(14) :: c4

   ! allocation of variables
   allocate ( b1(4), source = (/ child('abc','ABC'), child('def','DEF'), child('ghi','GHI'), child('jkl','JKL') /) )
   allocate ( b2(2,2), source = reshape (source = (/ child('ABC','abc'), child('DEF','def'), child('GHI','ghi'), child('JKL','jkl')  /), shape=(/2,2/) ) )
   allocate ( b3(4), source = (/ child('mno', 'MNO'), child('pqr', 'PQR'), child('stu', 'STU'), child('vwx','VWX') /) )
   allocate ( b4(2,2), source = reshape (source = (/ child('MNO', 'mno'), child('PQR','pqr'), child('STU', 'stu'), child('VWX','vwx')  /), shape=(/2,2/) ) )

   open (unit = 1, file ='array001a.data', form='unformatted', recl=50, access='direct')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec=4 )             b1((/3,2,4,1/))    !<- writes "ghiGHIZdefDEFZjklJKLZabcABCZ" (try array sectino with vector subscript)
   write (1, iostat=stat, iomsg=msg, rec=3 )             b2(1:2, 1)         !<- writes "ABCabcZDEFdefZ" (try array section with subscript triplet)
   write (1, iostat=stat, iomsg=msg, rec=2 )             b3(3)              !<- writes "stuSTUZ" (try array element)
   write (1, iostat=stat, iomsg=msg, rec=1 )             b4(1:2:2, 1:2:1)   !<- writes "MNOmnoZSTUstuZ" (try array section withe subscript triplet)

   read (1, iostat=stat, iomsg=msg, rec=1 )              c4
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c2
   read (1, iostat=stat, iomsg=msg, rec=4 )              c1

   ! check if the values are set correctly
   if ( c1 /= 'ghiGHIZdefDEFZjklJKLZabcABCZ' )  error stop 1_4
   if ( c2 /= 'ABCabcZDEFdefZ' )                error stop 2_4
   if ( c3 /= 'stuSTUZ' )                       error stop 3_4
   if ( c4 /= 'MNOmnoZSTUstuZ' )                error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

    select type (dtv)
       type is (child)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine
