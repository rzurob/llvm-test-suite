!* ===================================================================
!*
!* DATE                       : June 9, 2015
!*
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with Mold Expression
!* SECONDARY FUNCTIONS TESTED :
!*
!* REQUIRED COMPILER OPTIONS  :
!*
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS :
!*
!* DESCRIPTION                :
!*
!* TEST CASE ADAPTED FROM     : $(tsrcdir)F2008/AllocMold/func/AllocateWithSourceExp02-08.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithMoldExp02
      IMPLICIT NONE

!     INTEGER, ALLOCATABLE :: i(:)
      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A1(l1) = k1**2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        CLASS(Base(k2,:)), POINTER :: Cmp => null()
        INTEGER(k2) :: A2(2:l1,l2+1) = k1+k2 !10-1, 5+1 = 2:10, 1:6
      END TYPE Child

      TYPE(Child) :: c1, c2

      !call printAll("C1:", c1)
      IF ( SIZE(c1%A1)      .NE.    10    ) ERROR STOP 101
      IF ( ANY(SHAPE(c1%A1) .NE.  (/10/)) ) ERROR STOP 102
      IF ( RANK(c1%A1)      .NE.     1    ) ERROR STOP 103
      IF ( LBOUND(c1%A1,1)  .NE.     1    ) ERROR STOP 110
      IF ( UBOUND(c1%A1,1)  .NE.    10    ) ERROR STOP 120
      IF ( ANY(c1%A1        .NE.    16)   ) ERROR STOP 131
      IF ( c1%l1            .NE.    10    ) ERROR STOP 132
      IF ( SIZE(c1%A2)      .NE.    54    ) ERROR STOP 141
      IF ( ANY(SHAPE(c1%A2) .NE. (/9,6/)) ) ERROR STOP 142
      IF ( RANK(c1%A2)      .NE.     2    ) ERROR STOP 143
      IF ( LBOUND(c1%A2,1)  .NE.     2    ) ERROR STOP 151
      IF ( UBOUND(c1%A2,1)  .NE.    10    ) ERROR STOP 161
      IF ( LBOUND(c1%A2,2)  .NE.     1    ) ERROR STOP 152
      IF ( UBOUND(c1%A2,2)  .NE.     6    ) ERROR STOP 162
      IF ( ANY(c1%A2        .NE.     8)   ) ERROR STOP 171
      IF ( c1%l2            .NE.     5    ) ERROR STOP 172
      IF ( ASSOCIATED(c1%Cmp)             ) ERROR STOP 180

      ! TYPE(Child) :: c2
      !call printAll("C2:", c2)
      IF ( SIZE(c2%A1)      .NE.    10    ) ERROR STOP 301
      IF ( ANY(SHAPE(c2%A1) .NE.  (/10/)) ) ERROR STOP 302
      IF ( RANK(c2%A1)      .NE.     1    ) ERROR STOP 303
      IF ( LBOUND(c2%A1,1)  .NE.     1    ) ERROR STOP 310
      IF ( UBOUND(c2%A1,1)  .NE.    10    ) ERROR STOP 320
      IF ( ANY(c2%A1        .NE.    16)   ) ERROR STOP 330
      IF ( c2%l1            .NE.    10    ) ERROR STOP 332
      IF ( SIZE(c2%A2)      .NE.    54    ) ERROR STOP 341
      IF ( ANY(SHAPE(c2%A2) .NE. (/9,6/)) ) ERROR STOP 342
      IF ( RANK(c2%A2)      .NE.     2    ) ERROR STOP 343
      IF ( LBOUND(c2%A2,1)  .NE.     2    ) ERROR STOP 351
      IF ( UBOUND(c2%A2,1)  .NE.    10    ) ERROR STOP 361
      IF ( LBOUND(c1%A2,2)  .NE.     1    ) ERROR STOP 352
      IF ( UBOUND(c1%A2,2)  .NE.     6    ) ERROR STOP 362
      IF ( ANY(c2%A2        .NE.     8)   ) ERROR STOP 371
      IF ( c2%l2            .NE.     5    ) ERROR STOP 372
      IF ( ASSOCIATED(c2%Cmp)             ) ERROR STOP 380

      CALL sub2(c1, c2)

      SELECT TYPE ( s => c1%Cmp )
        CLASS IS (Child(4,*,4,*))
        !call printAll("C1%Cmp:", c1%cmp)
          IF ( SIZE(s%A1)      .NE.    10    ) ERROR STOP 201
          IF ( ANY(SHAPE(s%A1) .NE.  (/10/)) ) ERROR STOP 202
          IF ( RANK(s%A1)      .NE.     1    ) ERROR STOP 203
          IF ( LBOUND(s%A1,1)  .NE.     1    ) ERROR STOP 210
          IF ( UBOUND(s%A1,1)  .NE.    10    ) ERROR STOP 220
          IF ( ANY(s%A1        .NE.    16)   ) ERROR STOP 231
          IF ( s%l1            .NE.    10    ) ERROR STOP 232
          IF ( SIZE(s%A2)      .NE.    54    ) ERROR STOP 241
          IF ( ANY(SHAPE(s%A2) .NE. (/9,6/)) ) ERROR STOP 242
          IF ( RANK(s%A2)      .NE.     2    ) ERROR STOP 243
          IF ( LBOUND(s%A2,1)  .NE.     2    ) ERROR STOP 251
          IF ( UBOUND(s%A2,1)  .NE.    10    ) ERROR STOP 261
          IF ( LBOUND(s%A2,2)  .NE.     1    ) ERROR STOP 252
          IF ( UBOUND(s%A2,2)  .NE.     6    ) ERROR STOP 262
          IF ( ANY(s%A2        .NE.     8)   ) ERROR STOP 271
          IF ( s%l2            .NE.     5    ) ERROR STOP 272
          IF ( ASSOCIATED(s%Cmp)             ) ERROR STOP 280

        CLASS DEFAULT
           ERROR STOP 29
      END SELECT

      SELECT TYPE ( s => c2%Cmp )
        CLASS IS (Child(4,*,4,*))
        !call printAll("C2%Cmp:", c2%cmp)
          IF ( SIZE(s%A1)      .NE.   10     ) ERROR STOP 401
          IF ( ANY(SHAPE(s%A1) .NE. (/10/))  ) ERROR STOP 402
          IF ( RANK(s%A1)      .NE.    1     ) ERROR STOP 403
          IF ( LBOUND(s%A1,1)  .NE.    1     ) ERROR STOP 410
          IF ( UBOUND(s%A1,1)  .NE.   10     ) ERROR STOP 420
          IF ( ANY(s%A1        .NE.   16)    ) ERROR STOP 431
          IF ( s%l1            .NE.   10     ) ERROR STOP 432
          IF ( SIZE(s%A2)      .NE.   54     ) ERROR STOP 441
          IF ( ANY(SHAPE(s%A2) .NE. (/9,6/)) ) ERROR STOP 442
          IF ( RANK(s%A2)      .NE.    2     ) ERROR STOP 443
          IF ( LBOUND(s%A2,1)  .NE.    2     ) ERROR STOP 451
          IF ( UBOUND(s%A2,1)  .NE.   10     ) ERROR STOP 461
          IF ( LBOUND(s%A2,2)  .NE.    1     ) ERROR STOP 452
          IF ( UBOUND(s%A2,2)  .NE.    6     ) ERROR STOP 462
          IF ( ANY(s%A2        .NE.    8)    ) ERROR STOP 471
          IF ( s%l2            .NE.    5     ) ERROR STOP 472
          IF ( ASSOCIATED(s%Cmp)             ) ERROR STOP 480

        CLASS DEFAULT
           ERROR STOP 49
      END SELECT

      CONTAINS

      SUBROUTINE sub2(arg1, arg2)
        CLASS(base(4,*)) :: arg1, arg2
        CLASS(base(4,:)), POINTER :: tmp1
        CLASS(base(4,:)), POINTER :: tmp2

        SELECT TYPE ( arg1 )
          CLASS IS (Child(4,*,4,*))
            IF ( ASSOCIATED(arg1%Cmp) ) ERROR STOP 50
            SELECT TYPE ( arg2 )
              CLASS IS (Child(4,*,4,*))
                IF ( ASSOCIATED(arg1%Cmp) ) ERROR STOP 51
                ALLOCATE( tmp1, tmp2, MOLD = arg1 )
                arg1%cmp => tmp1
                arg2%cmp => tmp2
              CLASS DEFAULT
                ERROR STOP 54
            END SELECT
          CLASS DEFAULT
             ERROR STOP 55
        END SELECT
      END SUBROUTINE sub2

     ! FUNCTION checkShape(arr, i, val)
! !        INTEGER              :: di
       ! INTEGER     :: arr(*)
       ! INTEGER              :: i, val !index and value of element i
       ! LOGICAL              :: checkShape
       ! INTEGER, ALLOCATABLE :: e(:)

       ! print *, arr
! !      ALLOCATE(e, SOURCE=SHAPE(arr))
       ! checkShape = e(i) .NE. val
     ! END FUNCTION

      subroutine printAll(title, b4) !Used for debugging
        logical          :: dbg = .true.
        character(*)     :: title
        class(base(4,*)) :: b4

        if (dbg) then
          print*, title
          print*, "--------"

          print*; print *, "Base Part:";
          print*, "``````````"
          print*, "A1      :",  b4%A1
          print*, "size%A1 :", SIZE(b4%A1)
          print*, "shape%A1:", shape(b4%A1)
          print*, "lower%A1:", lbound(b4%A1, 1)
          print*, "upper%A1:", ubound(b4%A1, 1)

          select type (b4)
            class is (child(4,*,4,*))
              print*; print *, "Child Part:"
              print*, "``````````"
              print*, "A2      :", b4%A2
              print*, "size%A2 :", SIZE(b4%A2)
              print*, "shape%A2:", shape(b4%A2)
              print*, "lower%A2:", lbound(b4%A2,1)
              print*, "upper%A2:", ubound(b4%A2,1)
              print*, "lower%A2:", lbound(b4%A2,2)
              print*, "upper%A2:", ubound(b4%A2,2)

!             print*, "Component:", c1%Cmp
            class default
               error stop -999
          end select
          print*
        end if
      end subroutine



END PROGRAM AllocateWithMoldExp02
