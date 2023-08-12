! sqlite3_util.f90
!
! C interoperability interfaces and procedures.
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite3_util
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: i8 => int64, r8 => real64
    implicit none (type, external)
    private

    interface
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: str
            integer(kind=c_size_t)         :: c_strlen
        end function c_strlen
    end interface

    public :: c_f_str_ptr
contains
    subroutine c_f_str_ptr(c_str, f_str)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),                   intent(in)  :: c_str
        character(len=:), allocatable, intent(out) :: f_str

        character(kind=c_char), pointer :: ptrs(:)
        integer(kind=c_size_t)          :: i, sz

        copy_block: block
            if (.not. c_associated(c_str)) exit copy_block
            sz = c_strlen(c_str)
            if (sz < 0) exit copy_block
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(len=sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end block copy_block

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr
end module sqlite3_util
