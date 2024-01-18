!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS simply contiguous
!*                               6.5.4 & 12.5.2.7
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 CONTIGUOUS simply contig
!*                               Testing 6.5.4, 12.5.2.7 CONTIGUOUS
!*                               C1241
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous07d

        type base
          integer, pointer, contiguous :: ipac(:,:,:)
          integer, pointer             :: ipa (:,:,:)
        end type
        type(base)                     :: b

        integer, pointer, contiguous   :: ipac(:,:,:)
        integer, pointer               :: ipa (:,:,:)
        integer, target                :: ita (5,5,2)

        integer, pointer               :: map (:)

        ipac => ita

        ! 7.2.2.3.9 If bounds-remapping-list is specified, the pointer target shall
        !           be simply contiguous (6.5.4) or of rank one.

        ! 6.5.4.1
        ! A section-subscript-list specifies a simply contiguous section
        ! if and only if it does not have a vector subscript and
        ! map(1:10) => ipac([1,2,3,4,5],2)

        ! all but the last subscript-triplet is a colon,
        map(1:10) => ipac(:,:,1:2)

        ! the last subscript-triplet does not have a stride, and
        map(1:5) => ipac(:,:,1:2:2) ! Will fail.

        ! no subscript-triplet is preceded by a section-subscript that
        ! is a subscript.
        map(1:2) => ipac(:,1,1:2:1) ! Fail

        ! 6.5.4.2
        ! An array designator is simply contiguous if and only if it is
        ! an object-name that has the CONTIGUOUS attribute,
        map(1:10) => ipac
        map(1:10) => ipa ! Fail

        ! an object-name that is not a pointer or assumed-shape,
        map(1:10) => ita

        ! a structure-component whose final part-name is an array and
        ! that either has the CONTIGUOUS attribute
        ! or is not a pointer, or
        map(1:10) => b%ipac
        map(1:10) => b%ipa ! Fail

        ! an array section

        ! - that is not a complex-part-designator ,
        !     Not in plan:
        !     call simply_contig_sub(complextype%re)

        ! - that does not have a substring-range,
        !     Tested in contiguous06d

        ! - whose final part-ref has nonzero rank,
        map(1:1) => b%ipac(1,1,1) ! Fail

        ! - whose rightmost part-name has the CONTIGUOUS attribute or
        !   is neither assumed-shape nor a pointer,
        map(1:10) => b%ipac(:,:,1:2)
        map(1:10) => b%ipa(:,:,1:2)  ! Fail

        ! - which either does not have a section-subscript-list, or has
        !   a section-subscript-list which specifies a simply contiguous section.
        map(1:10) => b%ipac
        map(1:10) => b%ipac(:,:,1)


        ipac => ita
        ! 12.5.2.7: Pointer dummy variables:
        call simply_contig_sub(ipac)
        call simply_contig_sub(b%ipac)
        contains

        subroutine simply_contig_sub(ptr)
          ! 12.5.2.7: Pointer dummy variables:
          ! C1241: The actual argument corresponding to a dummy pointer
          !        with the CONTIGUOUS attribute shall be simply
          !        contiguous (6.5.4).
          integer, pointer, contiguous :: ptr (:,:,:)
        end subroutine
      end
