!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate003kl
!*
!*  PROGRAMMER                 : David Forster (derived from associate003 by Robert Ma)
!*  DATE                       : 2007-08-02 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Associate Construct
!*                                    -  selector is a function return with formatted i/o

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
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      function returnmyself(dtv)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         class(base(:)), allocatable :: returnmyself ! tcx: (:)

         allocate ( returnmyself, source = dtv )
      end function

end module

program associate003kl
   use m

   type(base(3)) :: b1(3)   = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   class(base(:)), allocatable :: b2(:) ! tcx: (:)
   type(child(3,4)) :: c1(2,2) ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   allocate ( b2(3), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   c1 = reshape ( source = (/ child(3,4)('ABC', 1001), child(3,4)('def', 1002), child(3,4)('GHI', 1003), child(3,4)('jkl', 1004)/), shape = (/2,2/) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'associate003kl.1', form='formatted', access='sequential' )


   associate ( a => b1(1)%returnmyself(), b => b2(1)%returnmyself(), c => c1(1,1)%returnmyself() )
      write ( 1, "(DT)", iostat = stat, iomsg = msg )    a
      write ( 1, "(DT)", iostat = stat, iomsg = msg )    b
      write ( 1, "(DT)", iostat = stat, iomsg = msg )    c
   end associate

   associate ( d => returnbase(b1), e => returnchild(c1) )
      write ( 1, "(3(DT))", iostat = stat, iomsg = msg ) d
      write ( 1, "(4(DT))", iostat = stat, iomsg = msg ) e
   end associate

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
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 13 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 8 changes
