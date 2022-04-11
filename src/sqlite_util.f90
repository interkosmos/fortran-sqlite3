! sqlite_util.f90
!
! C interoperability interfaces and procedures.
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite_util
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: i8 => int64, r8 => real64
    implicit none (type, external)
    private

    public :: c_f_str_ptr
    public :: c_strlen

    private :: copy

    interface
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: str
            integer(c_size_t)              :: c_strlen
        end function c_strlen
    end interface
contains
    pure function copy(a)
        character, intent(in)  :: a(:)
        character(len=size(a)) :: copy
        integer(kind=i8)       :: i

        do i = 1, size(a)
            copy(i:i) = a(i)
        end do
    end function copy

    subroutine c_f_str_ptr(c_str, f_str)
        type(c_ptr),                   intent(in)  :: c_str
        character(len=:), allocatable, intent(out) :: f_str
        character(kind=c_char), pointer            :: ptrs(:)
        integer(kind=i8)                           :: sz

        if (.not. c_associated(c_str)) return
        sz = c_strlen(c_str)
        if (sz < 0) return
        call c_f_pointer(c_str, ptrs, [ sz ])
        allocate (character(len=sz) :: f_str)
        f_str = copy(ptrs)
    end subroutine c_f_str_ptr
end module sqlite_util
