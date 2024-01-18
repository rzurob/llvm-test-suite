!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS
!*
!*  DESCRIPTION                : Testing proper functionality of
!*                               the F2008 intrinsic IS_CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module m
        integer, asynchronous, pointer, contiguous :: mipac(:)
      end module m

      module n
        integer, asynchronous, pointer, contiguous :: ipac(:)
      end module n

      program contiguous01f
        use m
        use n, nipac=>ipac

        use ISO_C_BINDING

        type empty
        end type
        type baseC
          integer                      :: bipc(9)
        end type
        type baseCP
          integer, pointer, contiguous :: ipac(:,:)
          integer, pointer             :: ipa (:,:)
        end type
        type baseII
          integer                      :: baseint
          integer                      :: contiguitystopper
        end type

        integer, pointer, contiguous       :: ipac   (:)
        integer, pointer, contiguous       :: ipac2d (:,:)
        integer, pointer                   :: ipa    (:)
        integer, pointer                   :: ipa2d  (:,:)
        integer, allocatable               :: iaa    (:)

        integer, target                    :: ita   (9)
        integer, target                    :: ita2d (5,2)
        integer, target                    :: itz   (0)

        character(:),  pointer, contiguous :: cpac (:)
        character(:),  pointer             :: cpa  (:)
        character(10), target              :: cta  (10)

        type(empty)                        :: e
        type(baseCP)                       :: b
        type(baseII), target               :: bt (5)
        type(baseC), pointer, contiguous   :: bpc(:)

        integer, volatile                          :: iva  (10)
        integer, asynchronous, pointer             :: iap  (:)
        integer, asynchronous, contiguous, pointer :: iacp (:)

        allocate(iaa(10))
        allocate(bpc(10))

        associate( x => ita )
          print *, IS_CONTIGUOUS(x), IS_CONTIGUOUS(ita) ! T T
        end associate
        associate( x => ita(1:9:2) )
          print *, IS_CONTIGUOUS(x), IS_CONTIGUOUS(ita(1:9:2)) ! F F
        end associate

        call sub_arg_contig_assumed (ita)
        call simply_contig_sub (b%ipac)

        print *, IS_CONTIGUOUS(iaa)
        print *, IS_CONTIGUOUS(bpc)

        ipac => ita
        cpac => cta

        !! 5: a pointer associated with a contiguous target, or
        ipa  => ipac ! ip is now contiguous
        cpa  => cpac ! cp is now contiguous
        print *, IS_CONTIGUOUS(ipa), IS_CONTIGUOUS(ipac)
        print *, IS_CONTIGUOUS(cpa), IS_CONTIGUOUS(cpac)

        ipac => ipa ! ip and cp are contiguous due to 5.7.3.2.5
        cpac => cpa
        print *, IS_CONTIGUOUS(ipa), IS_CONTIGUOUS(ipac)
        print *, IS_CONTIGUOUS(cpa), IS_CONTIGUOUS(cpac)

        ipac => ita ! it and ct are contiguous due to 5.7.3.2.2
        cpac => cta
        !print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(ita)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta)

        !! 6: a nonzero-sized array section (6.5.3) provided that:
        !! (a) its base object is contiguous,
        ipac => ita(1:5:1)
        print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(ita(1:5:1))

        ! (b) it does not have a vector subscript,
        print *, IS_CONTIGUOUS(ita([1,2,3,4,5]))

        ! (c) the elements of the section, in array element order, are
        !     a subset of the base object elements
        !     that are consecutive in array element order,
        ipac => ita(1:5)
        print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(ita(1:5))

        ! (d) if the array is of type character and a substring-range
        !     appears, the substring-range specifies all
        !     of the characters of the parent-string (6.4.1),
        cpac => cta(1:5)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:5))

        ! (e) only its final part-ref has nonzero rank, and
        !     scalar%scalar%array(section)
        ipac => bpc(1)%bipc(1:5)
        print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(bpc(1)%bipc(1:5))

        !! (f) it is not the real or imaginary part (6.4.4) of an array
        !!     of type complex.
        !!
        !! Feature not in plan.

        !! - the object has two or more elements,
        print *, IS_CONTIGUOUS(ipac), "!=", IS_CONTIGUOUS(bt%baseint)

        !! - the elements of the object in array element order are not
        !!   consecutive in the elements of the base object,
        print *, IS_CONTIGUOUS(ipac), "!=", IS_CONTIGUOUS(ita(5:1:-1))

        cpac => cta(1:0) ! Contiguous
        ipac => ita(1:0) ! Contiguous
        print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(ita(1:0))
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:0))

        !! - the object is not of a derived type that has no ultimate
        !!   components other than zero-sized arrays and characters
        !!   with length zero.
        ipac => itz ! Contiguous
        print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(itz)

        !! Full string
        cpac => cta(1:10)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:10))
        cpac => cta(1:10:1)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:10:1))
        cpac => cta(:)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(:))
        cpac => cta(1:2)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:2))

        ! Full strings again
        cpac => cta(1:10)(:)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:10)(:))
        cpac => cta(1:10:1)(:)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:10:1)(:))
        cpac => cta(:)(:)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(:)(:))
        cpac => cta(1:2)(:)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:2)(:))

        ! Substring (1:2)
        print *, IS_CONTIGUOUS(cta(1:10)(1:2))
        print *, IS_CONTIGUOUS(cta(1:10:1)(1:2))
        print *, IS_CONTIGUOUS(cta(1:10:2)(1:2))
        print *, IS_CONTIGUOUS(cta(:)(1:2))
        print *, IS_CONTIGUOUS(cta(1:2)(1:2))

        ! Full strings
        cpac => cta(1:10)(1:10)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:10)(1:10))
        cpac => cta(1:10:1)(1:10)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:10:1)(1:10))
        ! cpac => cta(1:10:2)(1:10)
        print *, IS_CONTIGUOUS(cta(1:10:2)(1:10))
        cpac => cta(:)(1:10)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(:)(1:10))
        cpac => cta(1:2)(1:10)
        print *, IS_CONTIGUOUS(cpac), IS_CONTIGUOUS(cta(1:2)(1:10))

        ! Substring (1:1)
        print *, IS_CONTIGUOUS(cta(1:10)(1:1))
        print *, IS_CONTIGUOUS(cta(1:10:1)(1:1))
        print *, IS_CONTIGUOUS(cta(1:10:2)(1:1))
        print *, IS_CONTIGUOUS(cta(:)(1:1))
        print *, IS_CONTIGUOUS(cta(1:2)(1:1))

        ! Empty substring (1:0)
        print *, IS_CONTIGUOUS(cta(1:10)(1:0))
        print *, IS_CONTIGUOUS(cta(1:10:1)(1:0))
        print *, IS_CONTIGUOUS(cta(1:10:2)(1:0))
        print *, IS_CONTIGUOUS(cta(:)(1:0))
        print *, IS_CONTIGUOUS(cta(1:2)(1:0))

        ipac => ita
        print *, IS_CONTIGUOUS(ipac), IS_CONTIGUOUS(ita)

        ! 6.5.4.1
        ! A section-subscript-list specifies a simply contiguous section
        ! if and only if it does not have a vector subscript and
        print *, IS_CONTIGUOUS(ipac2d([1,2,3,4,5],2))

        ! all but the last subscript-triplet is a colon,
        print *, IS_CONTIGUOUS(ipac2d(:,1:2))

        ! the last subscript-triplet does not have a stride, and
        print *, IS_CONTIGUOUS(ipac2d(:,1:2:2)) ! Will fail.

        ! no subscript-triplet is preceded by a section-subscript that
        ! is a subscript.
        print *, IS_CONTIGUOUS(ipac2d(1,1:2)) ! Fail

        ! 6.5.4.2
        ! An array designator is simply contiguous if and only if it is
        ! an object-name that has the CONTIGUOUS attribute,
        print *, IS_CONTIGUOUS(ipac)

        ! an object-name that is not a pointer or assumed-shape,
        print *, IS_CONTIGUOUS(ita)

        ! a structure-component whose final part-name is an array and
        ! that either has the CONTIGUOUS attribute
        ! or is not a pointer, or
        print *, IS_CONTIGUOUS(b%ipac)
        print *, IS_CONTIGUOUS(b%ipa) ! Fail: unassociated

        ! an array section

        ! - that is not a complex-part-designator ,
        !     Not in plan:
        !     call simply_contig_sub(complextype%re)

        ! - whose final part-ref has nonzero rank,
        print *, IS_CONTIGUOUS(bpc%bipc(1)) ! Fail

        ! - whose rightmost part-name has the CONTIGUOUS attribute or
        !   is neither assumed-shape nor a pointer,
        print *, IS_CONTIGUOUS(b%ipac(:,1:2))
        print *, IS_CONTIGUOUS(b%ipa(:,1:2))  ! Fail

        ! - which either does not have a section-subscript-list, or has
        !   a section-subscript-list which specifies a simply contiguous section.
        print *, IS_CONTIGUOUS(b%ipac)
        print *, IS_CONTIGUOUS(b%ipac(:,1))

        ! 12.5.2.7: Pointer dummy variables:
        call simply_contig_sub(ipac2d)
        call simply_contig_sub(b%ipac)
        ipac => ita

        call sub_arg_volatile(iva)         ! iv is simply contiguous
        call sub_arg_volatile(iva(1:10:1)) ! still simply contiguous

        call sub_arg_asynch(iacp,iacp) ! Has contiguous attr

        !call sub_arg_asynch(mipac,nipac) ! Has contiguous attr
        !call sub_arg_asynch(nipac,mipac) ! Has contiguous attr

      contains
        subroutine sub_arg_asynch(assumed, ptr)
          integer, asynchronous, contiguous           :: assumed (:)
          integer, asynchronous, contiguous, pointer  :: ptr (:)
          print *, IS_CONTIGUOUS(assumed), IS_CONTIGUOUS(ptr)
        end subroutine

        subroutine sub_arg_volatile(assumed)
          integer, volatile, contiguous               :: assumed (:)
          print *, IS_CONTIGUOUS(assumed)
        end subroutine

        subroutine simply_contig_sub(ptr)
          integer, pointer, contiguous                :: ptr (:,:)
          print *, IS_CONTIGUOUS(ptr)
        end subroutine

        subroutine sub_arg_contig_assumed (assumed)
          integer, contiguous                         :: assumed (:)
          print *, IS_CONTIGUOUS(assumed)
        end subroutine

        subroutine sub_arg_contig (assumed,assumed2,ptr)
          integer, contiguous                         :: assumed  (:)
          integer, contiguous                         :: assumed2 (0:)
          integer, contiguous, pointer                :: ptr (:)
          print *, IS_CONTIGUOUS(assumed), IS_CONTIGUOUS(assumed2), &
                   & IS_CONTIGUOUS(ptr)
        end subroutine

      end
