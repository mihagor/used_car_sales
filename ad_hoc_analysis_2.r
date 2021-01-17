# iz ad_hoc_analysis.R

sample_data %>% mutate(
      panoramska_streha = if_else(
            udobje_steklena_pomicna_streha_elektricni_pomik == 1 |
                  udobje_steklena_pomicna_streha == 1,
            1,
            0
      )
      ) %>% 
      count(cena, panoramska_streha) %>%
      arrange(desc(panoramska_streha), cena)
      
