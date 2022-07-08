# Cargar paquetes
library(pdftools)
library(tidyverse)
library(openxlsx)

# Cargar texto desde pdf
txt <- pdf_text("texto-definitivo-propuesta-nueva-constitucion.pdf")

# Ordenar texto en un data frame
df_const <- tibble()
for (i in 9:178){

  df_const <- bind_rows(df_const, 
                  tibble(strsplit(txt[i], "\nA")[[1]]) %>% 
                    rename(texto = 1) %>% 
                    mutate(art = paste0("A", str_sub(texto, 1, 11)),
                           art = str_remove(art, "\n"), 
                           texto = str_remove_all(texto, str_sub(texto, 1, 11)),
                           .before = 1) 
  )
  
  print(i)
}

# Vínculo capítulo-artículo
cap_art <- df_const %>% 
  filter(str_detect(texto, "CAPÍTULO ") | str_detect(art, "Artículo ")) %>% 
  mutate(cap = case_when(
    str_detect(texto, "CAPÍTULO ") ~ str_squish(str_remove_all(str_remove_all(texto, "\n"), "[0-9].*")))
  ) %>% 
  fill(cap, .direction = "down") %>% 
  select(cap, art) %>% 
  filter(str_detect(art, "Artículo ")) %>% 
  mutate(fila = row_number()) %>% 
  mutate(art = case_when(
    str_remove(art, "Artículo ") == as.character(fila) ~ art,
    TRUE ~ paste0("Artículo ", as.character(fila))
  )) %>% 
  select(-fila) %>% 
  mutate(cap = case_when(
    !str_detect(cap, "CONSTITUCIÓN POLÍTICA DE") ~ cap)
         ) %>% 
  fill(cap, .direction = "up")


# Detalle de artículos
df_const2 <- df_const %>% 
  filter(str_detect(art, "Artículo")) %>% 
  separate(texto, into = c("num1", "num2", "num3", "num4", 
                           "num5", "num6", "num7", "num8", 
                           "num9", "num10"), sep = "[0-9]+\\.") %>% 
  mutate(fila = row_number()) %>% 
  mutate(art = case_when(
    str_remove(art, "Artículo ") == as.character(fila) ~ art,
    TRUE ~ paste0("Artículo ", as.character(fila))
  )) %>% 
  select(-fila) %>% 
  pivot_longer(2:11, names_to = "num", values_to = "texto") %>% 
  mutate(texto = str_squish(texto)) %>% 
  filter(!is.na(texto)) %>% 
  filter(texto != "") %>% 
  group_by(art) %>% 
  mutate(num = row_number()) %>% 
  ungroup() %>% 
  mutate(texto = ifelse(str_sub(texto, 1, 1) == ".", str_sub(texto, 2, str_length(texto)), texto),
         texto = str_squish(texto))


left_join(cap_art, df_const2) %>% View

propuesta_df <- left_join(cap_art, df_const2)

propuesta_df %>% write.xlsx("PropuestaConstitucion2022.xlsx")
