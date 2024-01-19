! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-17 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2:
!*                               if input is a pointer, data are transferred from file to
!*                               the associated target.  If an output is a pointer, data shall
!*                               transfer from target to file. (WRITE, array pointer)
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
   type base (lbase_1) ! lbase_1=2
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(2) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(2), intent(in) :: char
      a%c = char
   end subroutine
end module


program pointer001al
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
   class(base(:)), pointer :: b1(:), b2(:,:) ! tcx: (:)
   class(base(:)), allocatable, target :: b3(:), b5(:) ! tcx: (:)
   type(base(:)), allocatable, target :: b4(:,:), b6(:,:) ! tcx: (:)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b3(4),   source = (/ base(2)('xx'), base(2)('xx'), base(2)('xx'), base(2)('xx') /)) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b4(2,2), source = reshape( source = (/ base(2)('xx'), base(2)('xx'), base(2)('xx'), base(2)('xx') /), shape=(/2,2/) ) ) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b5(4),   source = (/ base(2)('xx'), base(2)('xx'), base(2)('xx'), base(2)('xx') /)) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)
   allocate ( b6(2,2), source = reshape( source = (/ base(2)('xx'), base(2)('xx'), base(2)('xx'), base(2)('xx') /), shape=(/2,2/) ) ) ! tcx: (2) ! tcx: (2) ! tcx: (2) ! tcx: (2)

   b1 => b3
   b2 => b4

   open (unit = 1, file ='pointer001al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )            'abcdefgh'
   write (1, iostat=stat, iomsg=msg )            'ijklmnopqrstuvwy'
   write (1, iostat=stat, iomsg=msg )            'ABCD'
   write (1, iostat=stat, iomsg=msg )            'EFGHOPMNKLIJ'

   rewind 1

   read (1, iostat=stat, iomsg=msg )                        b1   !<- shall read the content to b3
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )            error stop 101_4
      if (( b1(1)%c /= 'ab') .or. ( b1(2)%c /= 'cd') .or.   &
          ( b1(3)%c /= 'ef') .or. ( b1(4)%c /= 'gh') )      error stop 2_4
      if (( b3(1)%c /= 'ab') .or. ( b3(2)%c /= 'cd') .or.   &
          ( b3(3)%c /= 'ef') .or. ( b3(4)%c /= 'gh') )      error stop 3_4
      msg = ''

   read (1, iostat=stat, iomsg=msg )                        b1, b2     !<- shall read the content to b3, b4 from file
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )            error stop 4_4
      if (( b1(1)%c /= 'ij') .or. ( b1(2)%c /= 'kl') .or.   &
          ( b1(3)%c /= 'mn') .or. ( b1(4)%c /= 'op') )      error stop 5_4
      if (( b3(1)%c /= 'ij') .or. ( b3(2)%c /= 'kl') .or.   &
          ( b3(3)%c /= 'mn') .or. ( b3(4)%c /= 'op') )      error stop 6_4
      if (( b2(1,1)%c /= 'qr') .or. ( b2(2,1)%c /= 'st') .or.   &
          ( b2(1,2)%c /= 'uv') .or. ( b2(2,2)%c /= 'wy') )  error stop 7_4
      if (( b4(1,1)%c /= 'qr') .or. ( b4(2,1)%c /= 'st') .or.   &
          ( b4(1,2)%c /= 'uv') .or. ( b4(2,2)%c /= 'wy') )  error stop 8_4
      msg = ''

   ! change pointer targets

   b1 => b5(1:4:2)                                           !<- points to b5(1) and b5(3)
   b2 => b6(2:1:-1,2:1:-1)                                   !<- points to b6 in reversed order 'OPMNKLIJ'

   read (1, iostat=stat, iomsg=msg )              b1          !<- shall read the content to b5 from file
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )            error stop 9_4
      if (( b1(1)%c /= 'AB') .or. ( b1(2)%c /= 'CD'))       error stop 10_4
      if (( b3(1)%c /= 'ij') .or. ( b3(2)%c /= 'kl') .or.   &
          ( b3(3)%c /= 'mn') .or. ( b3(4)%c /= 'op') )      error stop 11_4      !<- NO CHANGE
      if (( b5(1)%c /= 'AB') .or. ( b5(2)%c /= 'xx') .or.   &
          ( b5(3)%c /= 'CD') .or. ( b5(4)%c /= 'xx') )      error stop 12_4
      msg = ''

   read (1, iostat=stat, iomsg=msg )             b1, b2     !<- shall read the content to b5, b6 from file
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )            error stop 13_4
      if (( b1(1)%c /= 'EF') .or. ( b1(2)%c /= 'GH'))       error stop 14_4
      if (( b2(1,1)%c /= 'OP') .or. ( b2(2,1)%c /= 'MN') .or.   &
          ( b2(1,2)%c /= 'KL') .or. ( b2(2,2)%c /= 'IJ') )  error stop 15_4
      if (( b3(1)%c /= 'ij') .or. ( b3(2)%c /= 'kl') .or.   &
          ( b3(3)%c /= 'mn') .or. ( b3(4)%c /= 'op') )      error stop 16_4      !<- NO CHANGE
      if (( b4(1,1)%c /= 'qr') .or. ( b4(2,1)%c /= 'st') .or.   &
          ( b4(1,2)%c /= 'uv') .or. ( b4(2,2)%c /= 'wy') )  error stop 17_4      !<- NO CHANGE
      if (( b5(1)%c /= 'EF') .or. ( b5(2)%c /= 'xx') .or.   &
          ( b5(3)%c /= 'GH') .or. ( b5(4)%c /= 'xx') )      error stop 18_4
      if (( b6(1,1)%c /= 'IJ') .or. ( b6(2,1)%c /= 'KL') .or.   &
          ( b6(1,2)%c /= 'MN') .or. ( b6(2,2)%c /= 'OP') )  error stop 19_4
      msg = ''

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(2) :: temp
   read (unit, iostat=iostat ) temp

   dtv%c = temp

   iomsg = 'dtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (2) / declare with (*) - 23 changes
