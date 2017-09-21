tidy %>% filter(word  == "we") %>% group_by(gender) %>% count()

# men 1.48%
# women 1.53%

tidy %>% filter(word  == "our") %>% group_by(gender) %>% count()

# men  .70%
# women .85%

tidy %>% filter(word  == "us") %>% group_by(gender) %>% count()

# men .68%
# women .64%

