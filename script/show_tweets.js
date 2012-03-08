getTwitters('twitter', { 
  id: 'andrewheiss', 
  count: 1, 
  enableLinks: true, 
  ignoreReplies: true, 
  clearContents: true,
  template: '%text% <a href="http://twitter.com/%user_screen_name%/statuses/%id%/" class="twitter_timestamp">%time%</a>'
});