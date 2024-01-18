! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectType001cl
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate name (from select type construct)
!*                                 with unlimited polymorphic arrays being the selector
!*                               Sequential Access
!*
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
  contains
      procedure, pass :: get => getc
   end type

   type, extends(base) :: child
      character(lbase_1) :: cc = ''
   contains
      procedure, pass :: get => getcc
   end type

contains

   function getc (a)
      character(3) :: getc
      class(base(*)), intent(in) :: a ! tcx: (*)
      getc = a%c
   end function
   function getcc (a)
      character(3) :: getcc
      class(child(*)), intent(in) :: a ! tcx: (*)
      getcc = a%cc
   end function
end module

program selectType001cl
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(*), pointer      :: b1(:)
   class(*), allocatable  :: b2(:,:)
   class(*), allocatable  :: b3(:)
   integer :: stat
   character(200) :: msg
   character(14) :: c1
   character(28) :: c2
   character(4) :: c3

   ! allocation of variables

   allocate ( b1(3), source = (/ child(3)('xxx', 'xxx'), child(3)('xxx', 'xxx'), child(3)('xxx', 'xxx') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape( source = (/ child(3)('xxx','xxx'), child(3)('xxx','xxx'), & ! tcx: (3) ! tcx: (3)
                                child(3)('xxx','xxx'), child(3)('xxx','xxx') /), shape = (/2,2/)) ) ! tcx: (3) ! tcx: (3)
   allocate ( b3(3), source = (/ base(3)('xxx'), base(3)('xxx'), base(3)('xxx') /)) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='selectType001b.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )         'abcdefghijkl'
   write (1, iostat=stat, iomsg=msg )         'ABCDEFGHIJKLMNOPQRSTUVWX'
   write (1, iostat=stat, iomsg=msg )         'abcghi'

   rewind 1

   select type (b11 => b1(1:3:2) )
      class is (base(*)) ! tcx: (*)
         read (1, iostat=stat, iomsg=msg )    b11
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )                               error stop 101_4
         if (( b11(1)%c /= 'abc') .or. (b11(1)%get()  /= 'def') .or.              &
             ( b11(2)%c /= 'ghi') .or. (b11(2)%get()  /= 'jkl'))                  error stop 2_4     !<= get() calls getcc()
         msg = ''
      class default
         error stop 3_4
   end select

   select type (b12 => b2)
      class is (child(*)) ! tcx: (*)
         read (1, iostat=stat, iomsg=msg )    b12
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )                               error stop 4_4
         if (( b12(1,1)%c /= 'ABC') .or. (b12(1,1)%get()  /= 'DEF')  .or.         &
             ( b12(2,1)%c /= 'GHI') .or. (b12(2,1)%get()  /= 'JKL')  .or.         &
             ( b12(1,2)%c /= 'MNO') .or. (b12(1,2)%get()  /= 'PQR')  .or.         &
             ( b12(2,2)%c /= 'STU') .or. (b12(2,2)%get()  /= 'VWX') )             error stop 5_4     !<= get() calls getcc()
      class default
         error stop 6_4
   end select

   select type (b13 => b3(3:1:-2) )
      class is (base(*)) ! tcx: (*)
         read (1, iostat=stat, iomsg=msg )    b13
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )                               error stop 7_4
         if ((b13(1)%get()  /= 'abc')  .or.          &
             (b13(2)%get()  /= 'ghi') )                                           error stop 8_4
      class default
         error stop 9_4
   end select

   select type (b1)
      class is (base(*)) ! tcx: (*)
         if (( b1(1)%c /= 'abc') .or. (b1(1)%get()  /= 'def') .or.              &
             ( b1(2)%c /= 'xxx') .or. (b1(2)%get()  /= 'xxx') .or.              &
             ( b1(3)%c /= 'ghi') .or. (b1(3)%get()  /= 'jkl'))                  error stop 10_4
      class default
         error stop 11_4
   end select

   select type (b2)
      class is (child(*)) ! tcx: (*)
         if (( b2(1,1)%c /= 'ABC') .or. (b2(1,1)%get()  /= 'DEF')  .or.         &
             ( b2(2,1)%c /= 'GHI') .or. (b2(2,1)%get()  /= 'JKL')  .or.         &
             ( b2(1,2)%c /= 'MNO') .or. (b2(1,2)%get()  /= 'PQR')  .or.         &
             ( b2(2,2)%c /= 'STU') .or. (b2(2,2)%get()  /= 'VWX') )             error stop 12_4
      class default
         error stop 13_4
   end select

   select type (b3)
      class is (base(*)) ! tcx: (*)
         if ((b3(1)%get()  /= 'ghi')  .or.          &
             (b3(2)%get()  /= 'xxx')  .or.          &
             (b3(3)%get()  /= 'abc') )                                          error stop 14_4
      class default
         error stop 15_4
   end select

   !close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 ) error stop 16_4

    select type (dtv)
       type is (child(*)) ! tcx: (*)
          read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

   iomsg = 'dtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 11 changes
