/**
 * Frenetic language patterns
 *
 * @author Marco Canini
 * @version 1.0
 */
Rainbow.extend('frenetic', [
    {
        'name': 'keyword',
        'pattern': /\b(filter|true|all|fwd|not|learn|host|probe)\b/g
    },
    {
        'name': 'keyword.operator',
        'pattern': /:\=|\=&gt;|@|\?|\*|\=/g
    },
    {
        'name': 'constant.numeric',
        'pattern': /\b([0-9a-fA-F]+:[0-9a-fA-F]+:[0-9a-fA-F]+:[0-9a-fA-F]+:[0-9a-fA-F]+:[0-9a-fA-F]+|[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)\b/gi
    },
    {
        'name': 'support.type',
        'pattern': /\b(id|drop|switch|port|ethSrc|ethDst|ethTyp|vlanId|vlanPcp|ipSrc|ipDst|ipProto|tcpSrcPort|tcpDstPort|arp|ip|icmp|tcp|udp)\b/g
    }
]);
