" Replace bullet glyph with *
command! Replacebullet %s/[•|❒|❍]/*/g

" Snippet
command! Spoiler execute "normal! i<details><CR><Tab><summary><CR><TAB>Label<CR><Esc>ciw<Tab></summary><CR>Description<CR><Esc>ciw</details><CR><Esc>"

