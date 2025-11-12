\ core.fth - Minimal core words defined using primitives
\ This bootstraps the system so we can use : and ;

\ NOTE: This file cannot use : ; CONSTANT VARIABLE IF THEN etc.
\ We have to build them from scratch using primitives!

\ The challenge: How do we define : without having : ?
\ Answer: We can't easily. The bootstrap compiler needs to handle : and ;
\ OR we need to build up more infrastructure first.

\ For now, this file is empty - we need the bootstrap compiler to handle : ;
