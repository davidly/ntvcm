// This simplistic code extracts ASCII channel title, item title, and item description from RSS feeds.
// The intended consumer is low-end hardware from 1981 that can't deal with or non-original-ASCII characters.
// David Lee. May 2023.

#pragma once

#include <vector>
#include <algorithm>
#include <codecvt>
#include <locale>
#include <random>

#define USE_DEBUG_FILES false

#ifdef _MSC_VER
    #define OPEN_SOURCE_GETURL false   // this works on Windows too, but I'd rather avoid the dependencies
#else
    #define OPEN_SOURCE_GETURL true
#endif

#if OPEN_SOURCE_GETURL

    // Dependencies on:
    //     httplib.h from https://github.com/yhirose/cpp-httplib
    //     openssl headers and libraries

    #define CPPHTTPLIB_OPENSSL_SUPPORT
    #include "httplib.h"
    #pragma comment( lib, "libssl.lib" )
    #pragma comment( lib, "libcrypto.lib" )

#else

    #include <windows.h>
    #include <wininet.h>
    #pragma comment( lib, "wininet.lib" )
    
    class XIHandle
    {
        public:
            XIHandle( HANDLE h = 0 ) : _h(h) {}
            ~XIHandle() { Free(); }
            HANDLE Get() { return _h; }
            BOOL IsNull() { return 0 == _h; }
            void Free()
            {
                if ( 0 != _h )
                {
                    InternetCloseHandle( _h );
                    _h = 0;
                }
            }
        private:
            HANDLE _h;
    };

#endif

class CRssFeed
{
    private:
        struct RSSItem
        {
            string feed;
            string title;
            string description;
        };

        std::mutex mtx; // protect rssItems from multiple writers
        vector <RSSItem> rssItems;

        // strcpy is not guaranteed to work if portions of the strings overlap

        static void overlapping_strcpy( char * pto, const char * pfrom )
        {
            while ( *pfrom )
                *pto++ = *pfrom++;
            *pto = 0;
        } //overlapping_strcpy

        static const char * map_feed_to_file( const char * pfeed )
        {
            // these are for debugging to avoid hitting the servers -- open local snapshots of the files

            if ( strstr( pfeed, "nytimes" ) )
                return "nytimes.xml";
        
            if ( strstr( pfeed, "npr" ) )
                return "npr.xml";
        
            if ( strstr( pfeed, "guardian" ) )
                return "guardian.xml";
        
            return "wapost.xml";
        } //map_feed_to_file

        static string GetUrl( const string strUrl )
        {
            tracer.Trace( "geturl for %s\n", strUrl.c_str() );
            string response;
#if OPEN_SOURCE_GETURL
            // Use httplib.h from https://github.com/yhirose/cpp-httplib
            // That header pulls in a dependency on opensll -- so that must be in your include and lib paths too

            vector<char> feedname( 1 + strlen( strUrl.c_str() ) );
            strcpy( feedname.data(), strUrl.c_str() );
            char * pslash = strchr( feedname.data() + 8, '/' );
            if ( pslash )
            {
                *pslash = 0;
                httplib::Client cli( feedname.data() );
                const char * pslash2 = strchr( strUrl.c_str() + 8, '/' );
                if ( pslash2 )
                {
                    auto res = cli.Get( pslash2 );
                    response = res->body;
                 }
            }
#else
            XIHandle xhI( InternetOpenA( "djl_rssrdr", INTERNET_OPEN_TYPE_PRECONFIG, 0, 0, 0 ) );
            if ( !xhI.IsNull() )
            {
                XIHandle xhUrl( InternetOpenUrlA( xhI.Get(), strUrl.c_str(), "Accept: text/plain", 0,
                                                  INTERNET_FLAG_RELOAD | INTERNET_FLAG_DONT_CACHE |
                                                  INTERNET_FLAG_PRAGMA_NOCACHE | INTERNET_FLAG_NO_CACHE_WRITE |
                                                  INTERNET_FLAG_NO_COOKIES | INTERNET_FLAG_NO_UI | INTERNET_FLAG_SECURE,
                                                  0 ) );
                if ( !xhUrl.IsNull() )
                {
                    vector<char> buffer( 4096 );
                    do
                    {
                        DWORD cbRead = 0;
                        BOOL fOK = InternetReadFile( xhUrl.Get(), buffer.data(), (DWORD) ( buffer.size() - 1 ), &cbRead );
                        if ( fOK && ( 0 != cbRead ) )
                        {
                            buffer[ cbRead ] = 0;
                            response += buffer.data();
                        }
                        else
                            break;
                    } while( true );
                }
                else
                    tracer.Trace( "can't InternetOpenUrl, error %d\n", GetLastError() );
            }
            else
                tracer.Trace( "can't InternetOpenW, error %d\n", GetLastError() );
#endif
            return response;
        } //GetUrl

        static void replace_extended_characters( char * p )
        {
            // hack to solve a tiny percentage of cases based on actual usage by common RSS feeds.
            // many of these conversions are horrific, but it's all there is to work with.
            // the target machine is running CP/M from 1981, with US ascii support.
            // https://www.i18nqa.com/debug/utf8-debug.html

            while ( *p )
            {
                uint8_t x = (uint8_t) p[0];
                uint8_t one = (uint8_t) p[1];
                uint8_t two = (uint8_t) p[2];
        
                if ( 0xe2 == x && 0x80 == one)
                {
                    if ( 0x93 == two || 0x94 == two )
                        *p = '-';
                    else if ( 0x99 == two || 0x98 == two )
                        *p = '\'';
                    else if ( 0x9c == two || 0x9d == two )
                        *p = '"';
                    else
                    {
                        tracer.Trace( "unhandled character [%02x, %02x, %02x]", (uint8_t) p[0], (uint8_t) p[1], (uint8_t) p[2] );
                        *p = ' ';
                    }
        
                    overlapping_strcpy( p + 1, p + 3 );
                }
                else if ( 0xc2 == x || 0xc5 == x )
                {
                    *p = ' ';
                    overlapping_strcpy( p + 1, p + 2 );
                }
                else if ( 0xc3 == x ) // https://www.utf8-chartable.de/unicode-utf8-table.pl?start=128&number=128&names=-&utf8=0x
                {
                    if ( one >= 0x80 && one <= 0x85 )
                        *p = 'A';
                    else if ( 0x87 == one )
                        *p = 'C';
                    else if ( one >= 0x88 && one <= 0x8b )
                        *p = 'E';
                    else if ( one >= 0x8c && one <= 0x8f )
                        *p = 'I';
                    else if ( one >= 0x92 && one <= 0x96 )
                        *p = 'O';
                    else if ( one >= 0x99 && one <= 0x9c )
                        *p = 'U';
                    else if ( 0x9d == one )
                        *p = 'Y';
                    else if ( one >= 0xa1 && one <= 0xa5 )
                        *p = 'a';
                    else if ( 0xa7 == one )
                        *p = 'c';
                    else if ( one >= 0xa8 && one <= 0xab )
                        *p = 'e';
                    else if ( one >= 0xac && one <= 0xaf )
                        *p = 'i';
                    else if ( one >= 0xb2 && one <= 0xb6 )
                        *p = 'o';
                    else if ( one >= 0xb9 && one <= 0xbc )
                        *p = 'u';
                    else if ( 0xbd == one )
                        *p = 'y';
                    else
                    {
                        tracer.Trace( "unhandled character 0xc3[%02x]", one );
                        *p = ' ';
                    }
        
                    overlapping_strcpy( p + 1, p + 2 );
                }
        
                p++;
            }
        } //replace_extended_characters

        static string load_feed( const char * pfeed )
        {
            string response;
        
            if ( USE_DEBUG_FILES )
            {
                const char * pdebug_file = map_feed_to_file( pfeed );
                FILE * fp = fopen( pdebug_file, "r" );
                if ( !fp )
                    return response;
            
                fseek( fp, 0, SEEK_END );
                uint32_t file_size = ftell( fp );
                fseek( fp, 0, SEEK_SET );
        
                vector<char> file_data;
                file_data.resize( file_size + 1 );
                file_data[ file_size ] = 0;
                size_t x = fread( file_data.data(), file_size, 1, fp );
                fclose( fp );

                if ( x )
                    response = file_data.data();
            }
            else
                response = GetUrl( pfeed );
        
            return response;
        } //load_feed
        
        static void replace_string( char * p, const char * before, const char * after )
        {
            size_t blen = strlen( before );
            size_t alen = strlen( after );
        
            if ( blen < alen )
                return;
        
            while ( *p )
            {
                char * pnext = strstr( p, before );
        
                if ( !pnext )
                    break;
        
                strcpy( pnext, after );
                overlapping_strcpy( pnext + alen, pnext + blen );
                p = pnext + alen;
            }
        } //replace_string

        static void remove_tags( char * p )
        {
            do
            {
                char * ptag = strchr( p, '<' );
                if ( !ptag )
                    break;
                char * pend = strchr( ptag, '>' );
                if ( !pend )
                    break;

                overlapping_strcpy( ptag, pend + 1 );
            } while( true );
        } //remove_tags

        static string utf8_to_us_ascii( const char * utf8str )
        {
            wstring_convert<codecvt_utf8<wchar_t>> wconv;
            wstring wstr = wconv.from_bytes( utf8str );
            vector<char> buf( wstr.size() );
            const locale loc( ".20127" ); // ".20127" works on Windows but not on g++ & linux. "C" loses characters.
            use_facet<ctype<wchar_t>>( loc ).narrow( wstr.data(), wstr.data() + wstr.size(), ' ', buf.data() );
            return string( buf.data(), buf.size() );
        } //utf8_to_ascii
        
        static void make_ascii_string( char * p )
        {
#ifdef _MSC_VER
            string us_ascii = utf8_to_us_ascii( p );
            strcpy( p, us_ascii.c_str() );
#else // iconv is a non-portable option I didn't pursue
            replace_extended_characters( p );
#endif

            // change escaped characters

            replace_string( p, "&gt;", ">" );
            replace_string( p, "&lt;", "<" );
            replace_string( p, "&amp;", "&" );

            replace_string( p, ".</p>", ". " );
            replace_string( p, "</p>", ". " );
            remove_tags( p );
        } //make_ascii_string
        
        void parse_items( string response, const size_t max_item_size )
        {
            // Don't use an xml parser to reduce dependencies. this code assumes reasonably well-formed RSS.

            const char * p = response.c_str();
            vector<char> buf( 4096 );
        
            const char * pchannel = strstr( p, "<channel>" );
            if ( !pchannel )
                return;
        
            const char * pchannel_title = strstr( pchannel, "<title>" );
            if ( !pchannel_title )
                return;
        
            const char * pchannel_endtitle = strstr( pchannel_title, "</title>" );
            if ( !pchannel_endtitle )
                return;
        
            size_t channel_title_len = pchannel_endtitle - pchannel_title - 7;
            if ( channel_title_len >= buf.size() )
                return;
        
            memcpy( buf.data(), pchannel_title + 7, channel_title_len );
            buf[ channel_title_len ] = 0;
        
            make_ascii_string( buf.data() );
            string channel_title( buf.data() );
        
            do
            {
                const char * pitem = strstr( p, "<item>" );
                if ( !pitem )
                    break;
        
                const char * ptitle = strstr( pitem, "<title>" );
                if ( !ptitle )
                    break;
        
                const char * pendtitle = strstr( ptitle, "</title>" );
                if ( !pendtitle )
                    return;
        
                RSSItem item;
                item.feed = channel_title;
        
                size_t len = pendtitle - ptitle - 7;
                if ( len < buf.size() )
                {
                    memcpy( buf.data(), ptitle + 7, len );
                    buf[ len ] = 0;
                    make_ascii_string( buf.data() );
                    item.title = buf.data();
                }
                else
                {
                    tracer.Trace( "title is too long: %zd\n", len );
                    p = pendtitle;
                    continue;
                }
        
                const char * pdescription = strstr( pendtitle, "<description>" );
                if ( pdescription )
                {
                    const char * penddescription = strstr( pdescription, "</description>" );
                    if ( !penddescription )
                        return;
        
                    len = penddescription - pdescription - 13;
                    if ( len < buf.size() )
                    {
                        memcpy( buf.data(), pdescription + 13, len );
                        buf[ len ] = 0;
                        make_ascii_string( buf.data() );
                        item.description = buf.data();
                    }
                    else
                    {
                        tracer.Trace( "description is too long: %zd\n", len );
                        p = penddescription;
                        continue;
                    }
        
                    p = penddescription;
        
                    if ( ( item.feed.length() + item.title.length() + item.description.length() + 3 ) <= max_item_size )
                    {
                        lock_guard<mutex> lock( mtx );
                        rssItems.push_back( item );
                    }
                    else
                        tracer.Trace( "item is too long for buffer, ignoring\n" );
                }
                else
                    p = pendtitle;
            } while ( true );
        } //parse_items
        
    public:
        CRssFeed() {}
        ~CRssFeed() { clear(); }
        void clear() { rssItems.clear(); }
    
        size_t load_rss_feeds( char * feedv[10], const size_t max_item_size )
        {
            rssItems.clear();

            // get the count in a separate loop so OMP can work with a simple for loop

            int count = 0;
            for ( size_t feed_item = 0; ( 0 != feedv[ feed_item ] ); feed_item++ )
                count++;

            // use OMP to run all http get requests in parallel

            #pragma omp parallel for
            for ( int feed_item = 0; feed_item < count; feed_item++ )
            {
                string response = load_feed( feedv[ feed_item ] );
                parse_items( response, max_item_size );
            }

            std::random_device rd;
            std::mt19937 g(rd());
            std::shuffle( rssItems.begin(), rssItems.end(), g );
            return rssItems.size();
        } //load_rss_feeds
        
        bool fetch_rss_item( int i, char * item_buf, const size_t buffer_size )
        {
            // fill the buffer with 3 null-terminated strings for feed, title, and description

            item_buf[0] = item_buf[1] = item_buf[2] = 0;
        
            if ( i >= rssItems.size() || i < 0 )
                return false;
        
            RSSItem item = rssItems[ i ];
        
            size_t feed_len = strlen( item.feed.c_str() );
            size_t title_len = strlen( item.title.c_str() );
            size_t description_len = strlen( item.description.c_str() );
        
            if ( ( feed_len + title_len + description_len + 3 ) > buffer_size )
                return false;
        
            strcpy( item_buf, item.feed.c_str() );
            strcpy( item_buf + feed_len + 1, item.title.c_str() );
            strcpy( item_buf + feed_len + 1 + title_len + 1, item.description.c_str() );
            return true;
        } //fetch_rss_item
};    
        
