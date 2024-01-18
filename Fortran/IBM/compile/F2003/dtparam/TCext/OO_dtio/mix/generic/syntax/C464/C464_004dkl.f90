!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C464_004dkl
!*
!*  PROGRAMMER                 : David Forster (derived from C464_004d by Robert Ma)
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C464 If generic-spec is dtio-generic-spec, the interface of each binding
!*                                                  shall be as specified in 9.5.3.7. The type of the dtv argument
!*                                                  shall be type-name.
!*
!*                                             - Characteristics of the dummy arguments
!*                                                -> type, and type parameter
!*                               adaptation: exposed lengths
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

   type ::  base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
      contains

         procedure, pass :: uread
         procedure, pass :: uwrite
         procedure, pass :: fread
         procedure, pass :: fwrite

         generic, private :: read(unformatted)  => uread
         generic, private :: write(unformatted) => uwrite
         generic :: read(formatted)    => fread
         generic :: write(formatted)   => fwrite

   end type

   type :: dummy (ldummy_1) ! ldummy_1=3
      integer, len :: ldummy_1
      character(ldummy_1) :: c
   end type

   contains

      subroutine uread (dtv, unit, iostat, iomsg)
         integer, intent(inout) :: dtv             !<- intrinsic type
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

      end subroutine

      subroutine uwrite (dtv, unit, iostat, iomsg)
         class(dummy(*)), intent(in) :: dtv           !<- another derived type ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine fread (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer(8), intent(in) :: unit            !<- different kind type parameter
         character(*), intent(in) :: iotype
         integer(8), intent(in)  :: v_list(:)      !<- different kind type parameter
         integer(8), intent(out) :: iostat         !<- different kind type parameter
         character(*), intent(inout) :: iomsg

         read (unit,*, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine fwrite (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(100), intent(in) :: iotype      !<- different length type parameter
         real, intent(in)  :: v_list(:)            !<- different type
         integer, intent(out) :: iostat
         character(100), intent(inout) :: iomsg    !<- different length type parameter

         write (unit,*, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

end module

program C464_004dkl
end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 2 changes
! type: dummy - added parameters (ldummy_1) to invoke with (3) / declare with (*) - 1 changes
