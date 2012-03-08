module TextFilter
  def sort_by_key(hash)
    hash.keys.sort
  end
  
  def last_of_year(post)
    post['next'].nil? or post['next'].date.year != post['date'].year
  end
  
  def markdownify(input)
    text = Maruku.new(input)
    text.to_html
  end
  
  def slugify(input)
    input.gsub(' ', '-')
  end
  
  def descriptionify(excerpt)
    if excerpt.nil?
      description = 'Andrew Heiss is a student and web designer and developer from Utah who is working on his MPA in international nonprofit management at BYU.'
    else
      description = excerpt
    end
  
    "#{description}"
  end
  
  ############################################################
  # Developing methods. I don't really thinkg I need these…
  ############################################################
  
  def print_r(hash, level=0)
    result = "  "*level + "{\n"
    hash.keys.each do |key|
      result += "  "*(level+1) + "#{key} => "
      if hash[key].instance_of? Hash
        result += "\n" + print_r(hash[key], level+2)
      else 
        result += "#{hash[key]}\n"
      end
    end
    result += "  "*level + "}\n"
  end
  
  # Would be nice to do this with Hpricot or Tidy or something in the future, rather than parsing HTML with regex (evil, I know :) ). Like Henrick's - http://henrik.nyh.se/2008/01/rails-truncate-html-helper
  def close_tags(text)
    open_tags = []
    text.scan(/\<([^\>\s\/]+)[^\>\/]*?\>/).each { |t| open_tags.unshift(t) }
    text.scan(/\<\/([^\>\s\/]+)[^\>]*?\>/).each { |t| open_tags.slice!(open_tags.index(t)) }
    open_tags.each {|t| text += "</#{t}>" }
    text
  end
end

Liquid::Template.register_filter(TextFilter)