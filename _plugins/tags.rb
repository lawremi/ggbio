module Jekyll
  class RenderTimeTag < Liquid::Tag

    def initialize(tag_name, text, tokens)
      super
      @text = text
    end

    def render(context)
      "#{@text} #{Time.now}"
    end
  end
  
  class YearsSince < Liquid::Tag
    
    def initialize(tag_name, date, tokens)
      super
      @date = date.to_s
    end
    
    def render(context)
      today = Time.now
      my_year, my_month, my_day = @date.split("-")

      year_diff  = today.strftime("%Y").to_i - my_year.to_i
      month_diff = today.strftime("%m").to_i - my_month.to_i
      day_diff   = today.strftime("%d").to_i - my_day.to_i

      if month_diff < 0
         year_diff-=1
      elsif month_diff == 0 and day_diff < 0
         year_diff-=1
      end
      
      "#{year_diff}"
    end
  end
  
  class Random < Liquid::Tag
    def initialize(tag_name, max, tokens)
      super
      @max = max.to_i
    end
    
    def render(context)
      rand(@max).to_s
    end
  end
end

Liquid::Template.register_tag('random', Jekyll::Random)
Liquid::Template.register_tag('years_since', Jekyll::YearsSince)
Liquid::Template.register_tag('render_time', Jekyll::RenderTimeTag)