#Packages 
library('pacman')
p_load('tidyverse', 
       'geomtextpath',
       'reactable',
       'viridis', 
       'patchwork',  
      "ggtext", "mdthemes", 
       "glue", "scales")
#Theme design
my_theme <- function() { 
  
  theme_light()+
  theme(
    #panel.background = element_rect(fill = "#3E4A89FF"),
   # plot.background = element_rect(fill ="#3E4A89FF" ),
    panel.grid.major = element_blank(), 
    #axis.text.y = element_text(margin = margin(r = 50)),
    panel.grid.minor = element_blank(),
    plot.title = element_text( vjust = 0.5,
                              size=19,face ='bold', color = "black"),
    #plot.subtitle =   element_text(hjust = -.3, lineheight = 1, size=14),
    plot.subtitle = element_markdown(size =14,margin = margin(b = 20)),
    plot.caption =  element_text(hjust = 0, size=11),
    legend.position = c("none"),
   # panel.grid.major = element_line(size = 2),
    text = element_text(size=14),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold",color = "black"), 
    strip.text =  element_text(face = "bold",color = "black")   ) 
} 

#Diff between men and women 
 df_percep <- means_table_entidad_sex %>%  
  group_by(entidad_ab) %>%
  mutate(diff = round(last(ing_men) - first(ing_men), 1), 
         proporcion =  round((1-(last(ing_men) /(ing_men)))*100, 1)) %>%
  ungroup()

#Diff position
df_diff <- df_percep %>%  
  filter(sex == "Hombre") %>% 
  mutate(x_position = ing_men + (diff/2))

plot_ingreso_sexo <- means_table_entidad_sex %>%
  ggplot(aes(x= ing_men, y= entidad_ab)) +
  geom_line(aes(group = entidad_ab) ,color="#aeb6bf", alpha= .5 , size = 7)+ 
  geom_point( aes(color = sex),size=12,alpha  = .9) +
  geom_text( data =  means_table_entidad_sex   |>  filter(entidad_ab != 'CHIS') ,
             aes(label= (paste0("",round(ing_men/1000,1), "K"))), 
            nudge_x = 1, nudge_y = 0, size = 4.5)+
  geom_text(data = df_diff,
            aes(label = paste0("-%",proporcion), 
                x = x_position, y =reorder(entidad_ab,entidad_ab)), 
            #fill =   "white",
            color = "brown4",
            alpha = .8,
            size = 5) + 
  labs(y="",
       x= "Ingreso laboral promedio mensual ($/MXN)",
       title = "Brecha de ingresos laborales entre mujeres y hombres por entidad (2023 4T)",
       subtitle =  paste0("A nivel nacional, el ingreso promedio mensual de las 
                            <span style='color: salmon;'>**mujeres es de $7,609.8**</span>,
                            y para los <span style='color: #1F9E89FF;'>**hombres de $9,805.4**</span>  
              En promedio, el ingreso laboral de las <span style='color: #FA8072;'>**mujeres**</span> 
                                          es <span style='color: brown4;'>**22.3%**</span>
                          menor que el ingreso de los <span style='color: #1F9E89FF;'>**hombres**</span>  ") ,
       caption="Fuente: Estimaciones propias con datos de la ENOE 2023 2T | @Diegosreco  Nota: En gris la brecha del ingreso laboral total en porcentaje")  +
  my_theme() +
  scale_color_manual(name = "Categoría", values = c(pal))  


plot_ingreso_sexo









pal <- (c("#1F9E89FF",  "#FA8072"))
