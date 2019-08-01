## Author: Jc J
## Date: July 26, 2019
##
## Utility functions for the mergesort program
##

.globl util.clone_list
.globl util.print_list
.globl util.print_list_range
.globl util.print_char_n_times
.globl util.memcpy

.text

## create_list
## NOTE: This function is unused in this program.
##
## Creates a dynamic list on the heap n elements in size.
## e.g. if n is 3, the function returns a heap address to the following list:
## { 3, *elements } where elements = [0, 0, 0]
##
## Arguments:
##		a0:		number of elements
##
## Register usage:
##		s0:		number of elements
##		s1:		address of the result list
##
util.create_list:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		sw			$s0, 24($sp)
		sw			$s1, 20($sp)
		
		move		$s0, $a0								# s0: number of elements
		
		li			$a0, 8									# allocate space on the heap for the list structure
		li			$v0, 9
		syscall
		move		$s1, $v0								# s1: address of created list
		sw			$s0, 0($s1)								# set list.n_elements

		mul			$a0, $s0, 4								# allocate n_elements * 4 bytes of space on the heap
		li			$v0, 9									# for list.elements
		syscall
		sw			$v0, 4($s1)								# set pointer to elements
		
		move		$v0, $s1								# return the address to the created list
		
		lw			$s0, 24($sp)
		lw			$s1, 20($sp)
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of create_list


## clone_list : void
##
## Clones one list to another. Both lists must be the same size.
##
## Arguments:
##		a0:		address of source list
##		a1:		address of destination list
##
util.clone_list:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		lw			$a2, 0($a0)								# 3rd arg: n_elements * 4 bytes
		mul			$a2, $a2, 4	
		
		lw			$a0, 4($a0)								# 1st arg: &src.elements
		lw			$a1, 4($a1)								# 2nd arg: &dst.elements
			
		jal			util.memcpy								# memcpy(src, dst, n_bytes)
				
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of clone_list


## print_list
##
## Pretty prints all elements of a list. e.g. [1, 2, 3, 4]
##
## Arguments:
##		a0:		address of the list structure to print
##				lists are defined as { n_elements, *elements }
##
util.print_list:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
															# 1st arg: list address (a0)
		li			$a1, 0									# 2nd arg: 0 (start index)
		lw			$a2, 0($a0)								# 3rd arg: list->n_elements (end index)
		jal			util.print_list_range
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of print_list


## print_list_range
##
## Pretty prints a range [start, end) of elements of a list.
##
## Arguments:
##		a0:		address of the list structure to print
##				lists are defined as { n_elements, *elements }
##		a1:		start index
##		a2:		end index (non inclusive)
##
## Register usage:
##		s0:		offset into list.elements
##		s1:		sentinel for final index (determines whether to print comma)
##				scratch space
##
util.print_list_range:

		subu		$sp, $sp, 32								
		sw			$ra, 28($sp)
		
		sw			$s0, 24($sp)
		sw			$s1, 20($sp)
		
		mul			$s1, $a1, 4									# s1: offset into list.elements
		lw			$s0, 4($a0)									# s0: list.elements
		addu		$s0, $s0, $s1								# s0: list.elements[offset]
		
		subi		$s1, $a2, 1									# s1: last index of list.elements
		
		li			$a0, 0x5b									# print "["
		li			$v0, 11
		syscall
		
	plr_loop:
	
		beq			$a1, $a2, plr_loop_end						# if start == end, we're done.
		
		lw			$a0, ($s0)									# print list.elements[offset]
		li			$v0, 1
		syscall

		beq			$a1, $s1, plr_skip_comma 					# if the last element, skip printing a comma
		
		li			$a0, 0x2c									# else print ", "
		li			$v0, 11
		syscall
		li			$a0, 0x20
		li			$v0, 11
		syscall
		
	plr_skip_comma:
	
		addu		$s0, $s0, 4									# list.elements++
		addi		$a1, $a1, 1									# increment offset
		b			plr_loop
		
	plr_loop_end:
	
		li			$a0, 0x5d									# print "]"
		li			$v0, 11
		syscall		
		
		lw 			$s0, 24($sp)								
		lw			$s1, 20($sp)
		
		lw			$ra, 28($sp)								
		addu		$sp, $sp, 32		
		jr			$ra
## end of print_list


## print_char_n_times
##
## Arguments:
##		a0:		character to print (byte)
##		a1:		number of times (word)
##
util.print_char_n_times:

	pcnt_loop:
	
		beqz		$a1, pcnt_loop_end							# if n == 0, return
		li			$v0, 11										# print the character
		syscall
		subi		$a1, $a1, 1									# n--
		b			pcnt_loop
	
	pcnt_loop_end:
		
		jr			$ra
## end of print_char_n_times


## memcpy : void
##
## Copies the contents of memory from one location to another
##
## Arguments:
##		a0:		source address
##		a1:		destination address
##		a2:		n_bytes
##
## Register usage:
##		s0:		byte being copied
##
util.memcpy:

		subu		$sp, $sp, 32
		sw			$s0, 28($sp)
		
	c_loop:			
												
		beqz		$a2, c_loop_end								# while there are still bytes to copy
		lb			$s0, ($a0)									# copy the byte from src to dst
		sb			$s0, ($a1)
		
		addu		$a0, $a0, 1									# increase src and dst pointers by one byte
		addu		$a1, $a1, 1
		
		sub			$a2, $a2, 1									# n_bytes--
		b			c_loop
		
	c_loop_end:
	
		lw			$s0, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of memcpy
