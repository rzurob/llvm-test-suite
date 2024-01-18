!*  ===================================================================
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Associate Construct
!*                                    -  selector is a function return with unformatted i/o

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

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: returnmyself
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted) => read
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      function returnmyself(dtv)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         class(base(:)), allocatable :: returnmyself ! tcx: (:)
         allocate ( returnmyself, source = dtv )
      end function

end module

program associate003akl
   use m

   type(base(3)) :: b1(3)   = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   class(base(:)), allocatable :: b2(:) ! tcx: (:)
   type(child(3,4)) :: c1(2,2) ! tcx: (3,4)

   type(base(3))  :: b11, b12, b13(3) ! tcx: (3)
   type(child(3,4)) :: c11, c12(4) ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   allocate ( b2(3), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   c1 = reshape ( source = (/ child(3,4)('ABC', 1001), child(3,4)('def', 1002), child(3,4)('GHI', 1003), child(3,4)('jkl', 1004)/), shape = (/2,2/) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'associate003akl.1', form='unformatted', access='sequential' )


   associate ( a => b1(1)%returnmyself(), b => b2(1)%returnmyself(), c => c1(1,1)%returnmyself() )
      write ( 1, iostat = stat, iomsg = msg )    a
      write ( 1, iostat = stat, iomsg = msg )    b
      write ( 1, iostat = stat, iomsg = msg )    c
   end associate

   associate ( d => returnbase(b1), e => returnchild(c1) )
      write ( 1, iostat = stat, iomsg = msg ) d
      write ( 1, iostat = stat, iomsg = msg ) e
   end associate

   rewind 1

   read (1, iostat = stat, iomsg = msg ) b11
   read (1, iostat = stat, iomsg = msg ) b12
   read (1, iostat = stat, iomsg = msg ) c11
   read (1, iostat = stat, iomsg = msg ) b13
   read (1, iostat = stat, iomsg = msg ) c12

   if ( ( b11%c /= 'abc') .or. ( b12%c /= 'ABC' ) .or. ( c11%c /= 'ABC' ) .or. ( c11%i /= 1001 ) ) error stop 1_4
   if ( ( b13(1)%c /= 'abc' ) .or. ( b13(2)%c /= 'def' ) .or. ( b13(3)%c /= 'ghi' ) )              error stop 2_4
   if ( ( c12(1)%c /= 'ABC' ) .or. ( c12(2)%c /= 'def' ) .or. ( c12(3)%c /= 'GHI' ) .or. ( c12(4)%c /= 'jkl' ) .or. &
        ( c12(1)%i /= 1001 ) .or. ( c12(2)%i /= 1002 ) .or. ( c12(3)%i /= 1003 ) .or. ( c12(4)%i /= 1004 ))   error stop 3_4

   close ( 1, status ='delete')

   contains

      elemental function returnbase(dtv)
         type(base(*)), intent(in) :: dtv ! tcx: (*)
         type(base(3)) :: returnbase ! tcx: (3)
         returnbase = dtv
      end function

      elemental function returnchild(dtv)
         type(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         type(child(3,4)) :: returnchild ! tcx: (3,4)
         returnchild = dtv
      end function

end program



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 15 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 10 changes
